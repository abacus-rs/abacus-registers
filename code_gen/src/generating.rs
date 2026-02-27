use crate::parsing::{State, StateDefinition};
use quote::{format_ident, quote};
use syn::Ident;

struct GeneratedStateType {
    generic_parameter_list: Option<Vec<Ident>>,
    type_signature: proc_macro2::TokenStream,
}

impl GeneratedStateType {
    /// Create a new GeneratedStateType from a State.
    ///
    /// Consumes parsed State and forms the Ident that serves as
    /// the corresponding type signature.
    fn new(state: State) -> Self {
        let state_ident = &state.state_name;
        let mut generics_list = Vec::new();

        let type_signature = if state.substates.is_empty() {
            quote! { #state_ident }
        } else {
            // We need to replace the `Any` keyword with a generic
            // that is incremented for each (e.g. State<SubState, Any, Any>
            // must become State<SubState, T0: SubState, T1: SubState>)
            // For type args we use just the ident (T0), not T0: SubState - bounds belong on impl.
            let args: Vec<proc_macro2::TokenStream> = if state.contains_any() {
                let mut generic_count: usize = 0;
                state
                    .substates
                    .iter()
                    .map(|substate| {
                        if substate.to_string() == "Any" {
                            let generic_ident = format_ident!("T{}", generic_count);
                            generics_list.push(generic_ident.clone());
                            generic_count += 1;
                            quote! { #generic_ident }
                        } else {
                            quote! { #substate }
                        }
                    })
                    .collect()
            } else {
                state
                    .substates
                    .iter()
                    .map(|substate| quote! { #substate })
                    .collect()
            };
            quote! { #state_ident<#(#args),*> }
        };

        if generics_list.is_empty() {
            Self {
                generic_parameter_list: None,
                type_signature,
            }
        } else {
            Self {
                generic_parameter_list: Some(generics_list),
                type_signature,
            }
        }
    }
}

impl State {
    /// Check if a state contains any substate named "Any".
    pub fn contains_any(&self) -> bool {
        self.substates
            .iter()
            .any(|substate| substate.to_string() == "Any")
    }

    /// Produces the type signature token stream for this state (e.g. `Off`, `Active<Tx, Rx>`,
    /// or `Active<T0, T1>` when substates contain `Any`).
    pub fn form_state_type_signature_token(&self) -> proc_macro2::TokenStream {
        GeneratedStateType::new(self.clone()).type_signature
    }

    /// When state contains `Any`, returns impl generics (e.g. `T0: SubState, T1: SubState`)
    /// for wrapping impl blocks. Returns None when no generics needed.
    pub fn form_impl_generics_token(&self) -> Option<proc_macro2::TokenStream> {
        let gen = GeneratedStateType::new(self.clone());
        gen.generic_parameter_list.map(|params| {
            quote! { #(#params: SubState),* }
        })
    }

    /// Marker type for this state; used for state_map keys and register bindings.
    /// Same as type signature for hashing and quote! use.
    pub fn form_state_marker_type(&self) -> proc_macro2::TokenStream {
        self.form_state_type_signature_token()
    }

    /// Concrete state type (with `Any` replaced by generics); used for duplicate detection and in quote!.
    pub fn form_concrete_state_type(&self) -> proc_macro2::TokenStream {
        self.form_state_type_signature_token()
    }
}

/// Generates an `Enum` for all hardware states and implements the `StateEnum` trait
/// (including `sync_state` for transient states).
pub fn generate_state_enum(
    state_definition: &Vec<StateDefinition>,
    register_name: &Ident,
    store_name: &Ident,
) -> proc_macro2::TokenStream {
    let mut output = proc_macro2::TokenStream::new();

    // Create enum variant for a given state.
    let store_variants = state_definition.iter().map(|state_def| {
        let variant_name = &state_def.state_shortname;
        let state_type = state_def.state.form_state_type_signature_token();
        quote! { #variant_name(#register_name<#state_type>) }
    });

    let sync_state_variants = state_definition.iter().map(|state_def| {
        let variant_name = state_def.state_shortname.clone();
        // For transient states: call reg.sync_state(); for non-transient: identity.
        if state_def.transient {
            quote! { #store_name::#variant_name(reg) => reg.sync_state() }
        } else {
            quote! { #store_name::#variant_name(x) => #store_name::#variant_name(x) }
        }
    });

    output.extend(quote! {
        pub enum #store_name{
            #(#store_variants),*
        }

        impl StateEnum for #store_name {
            fn sync_state(self) -> Self {
                match self {
                    #(#sync_state_variants),*
                }
            }
        }
    });

    output
}

/// Generates `State`, `Reg`, `From`, and `TryFrom` impls for a given state.
/// Does not emit the state struct (with PhantomData); that is produced by the caller (e.g. lib.rs).
pub fn generate_state(
    state_definition: &StateDefinition,
    state_enum_name: &Ident,
    register_name: &Ident,
) -> proc_macro2::TokenStream {
    let mut result = proc_macro2::TokenStream::new();

    let state_shortname = &state_definition.state_shortname;

    // Form struct name / generics in angle brackets
    let state_type_signature_ident = state_definition.state.form_state_type_signature_token();
    let impl_generics = state_definition.state.form_impl_generics_token();

    let (impl_state, impl_reg, impl_from, impl_try_from) = if let Some(generics) = impl_generics {
        (
            quote! { impl<#generics> State for #state_type_signature_ident },
            quote! { impl<#generics> Reg for #register_name<#state_type_signature_ident> },
            quote! { impl<#generics> From<#register_name<#state_type_signature_ident>> for #state_enum_name },
            quote! { impl<#generics> TryFrom<#state_enum_name> for #register_name<#state_type_signature_ident> },
        )
    } else {
        (
            quote! { impl State for #state_type_signature_ident },
            quote! { impl Reg for #register_name<#state_type_signature_ident> },
            quote! { impl From<#register_name<#state_type_signature_ident>> for #state_enum_name },
            quote! { impl TryFrom<#state_enum_name> for #register_name<#state_type_signature_ident> },
        )
    };

    result.extend(quote! {
            #impl_state {
            }

            #impl_reg {
                type StateEnum = #state_enum_name;

            }

    });

    result.extend(quote! {
        #impl_from {
            fn from(reg: #register_name<#state_type_signature_ident>) -> Self {
                #state_enum_name::#state_shortname(reg)
            }
        }

        #impl_try_from {
            type Error = #state_enum_name;
            fn try_from(store: #state_enum_name) -> Result<Self, Self::Error> {
                match store {
                    #state_enum_name::#state_shortname(reg) => Ok(reg),
                    _ => Err(store),
                }
            }
        }
    });

    // Transient states must implement SyncState (enforced via TransientStateReg supertrait).
    if state_definition.transient {
        let impl_transient = match state_definition.state.form_impl_generics_token() {
            Some(generics) => quote! { impl<#generics> TransientStateReg for #register_name<#state_type_signature_ident> },
            None => quote! { impl TransientStateReg for #register_name<#state_type_signature_ident> },
        };
        result.extend(quote! {
            #impl_transient {}
        });
    }

    result
}

/// Generates Reg, From, and TryFrom impls (not State). Used for concrete states that have
/// substates (e.g. Active<RxIdle, TxIdle>). State comes from the generic (Any, Any) impl.
/// StepOff and similar traits require Reg.
pub fn generate_state_conversion_impls_only(
    state_definition: &StateDefinition,
    state_enum_name: &Ident,
    register_name: &Ident,
) -> proc_macro2::TokenStream {
    let state_shortname = &state_definition.state_shortname;
    let state_type_signature_ident = state_definition.state.form_state_type_signature_token();

    let mut result = proc_macro2::TokenStream::new();
    result.extend(quote! {
        impl Reg for #register_name<#state_type_signature_ident> {
            type StateEnum = #state_enum_name;
        }

        impl From<#register_name<#state_type_signature_ident>> for #state_enum_name {
            fn from(reg: #register_name<#state_type_signature_ident>) -> Self {
                #state_enum_name::#state_shortname(reg)
            }
        }

        impl TryFrom<#state_enum_name> for #register_name<#state_type_signature_ident> {
            type Error = #state_enum_name;
            fn try_from(store: #state_enum_name) -> Result<Self, Self::Error> {
                match store {
                    #state_enum_name::#state_shortname(reg) => Ok(reg),
                    _ => Err(store),
                }
            }
        }
    });

    if state_definition.transient {
        result.extend(quote! {
            impl TransientStateReg for #register_name<#state_type_signature_ident> {}
        });
    }

    result
}

/// Generates only `State` impl (no Reg/From/TryFrom). Used for the fully generic (Any, Any)
/// state to satisfy `Active<T0, T1>: State` in register bindings. Reg/From/TryFrom are
/// provided by concrete-state impls only to avoid E0119 overlap (generic TryFrom would
/// conflict with concrete TryFrom).
pub fn generate_state_trait_impls_only(
    state_definition: &StateDefinition,
    _register_name: &Ident,
    _state_enum_name: &Ident,
) -> proc_macro2::TokenStream {
    let state_type_signature_ident = state_definition.state.form_state_type_signature_token();
    let impl_generics = state_definition.state.form_impl_generics_token();

    let impl_state = if let Some(generics) = impl_generics {
        quote! { impl<#generics> State for #state_type_signature_ident }
    } else {
        quote! { impl State for #state_type_signature_ident }
    };

    quote! {
        #impl_state {}
    }
}

// Based on generated output from tock/chips/nrf5x/src/temperature.rs and tock/chips/nrf52/src/uart.rs
#[cfg(test)]
mod tests {
    use super::*;
    use syn::parse_quote;

    /// State definitions matching the Nrf5xTemp temperature sensor (Off, Reading).
    fn nrf5x_temp_state_definitions() -> Vec<StateDefinition> {
        vec![
            {
                let state: State = parse_quote!(Off);
                StateDefinition {
                    state,
                    transient: false,
                    state_shortname: parse_quote!(Off),
                }
            },
            {
                let state: State = parse_quote!(Reading);
                StateDefinition {
                    state,
                    transient: false,
                    state_shortname: parse_quote!(Reading),
                }
            },
        ]
    }

    /// UART-style state definitions (Off + Active variants with shortnames ActiveIdle, ActiveRx, ActiveTx, ActiveRxTx).
    fn nrf52_uarte_state_definitions() -> Vec<StateDefinition> {
        vec![
            {
                let state: State = parse_quote!(Off);
                StateDefinition {
                    state,
                    transient: false,
                    state_shortname: parse_quote!(Off),
                }
            },
            {
                let state: State = parse_quote!(Active(RxIdle, TxIdle));
                StateDefinition {
                    state,
                    transient: false,
                    state_shortname: parse_quote!(ActiveIdle),
                }
            },
            {
                let state: State = parse_quote!(Active(Transient, TxIdle));
                StateDefinition {
                    state,
                    transient: false,
                    state_shortname: parse_quote!(ActiveRx),
                }
            },
            {
                let state: State = parse_quote!(Active(RxIdle, Transient));
                StateDefinition {
                    state,
                    transient: false,
                    state_shortname: parse_quote!(ActiveTx),
                }
            },
            {
                let state: State = parse_quote!(Active(Transient, Transient));
                StateDefinition {
                    state,
                    transient: false,
                    state_shortname: parse_quote!(ActiveRxTx),
                }
            },
        ]
    }

    #[test]
    fn test_nrf5x_temp_generate_state_enum() {
        let state_defs = nrf5x_temp_state_definitions();
        let register = format_ident!("Nrf5xTempRegisters");
        let store = format_ident!("Nrf5xTempStateEnum");

        let out = generate_state_enum(&state_defs, &register, &store);
        let s = out.to_string();

        assert!(s.contains("Nrf5xTempStateEnum"));
        assert!(s.contains("Nrf5xTempRegisters"));
        assert!(s.contains("Off"));
        assert!(s.contains("Reading"));
        assert!(s.contains("StateEnum"));
        assert!(s.contains("sync_state"));
    }

    #[test]
    fn test_nrf5x_temp_generate_state_for_off_and_reading() {
        let state_defs = nrf5x_temp_state_definitions();
        let register = format_ident!("Nrf5xTempRegisters");
        let store = format_ident!("Nrf5xTempStateEnum");

        let mut full = proc_macro2::TokenStream::new();
        for state_def in &state_defs {
            full.extend(generate_state(state_def, &store, &register));
        }
        let s = full.to_string();

        assert!(s.contains("impl State for Off"));
        assert!(s.contains("impl State for Reading"));
        assert!(s.contains("impl Reg for Nrf5xTempRegisters"));
        assert!(s.contains("From"));
        assert!(s.contains("TryFrom"));
        assert!(s.contains("Nrf5xTempStateEnum") && s.contains("Off"));
        assert!(s.contains("Nrf5xTempStateEnum") && s.contains("Reading"));
    }

    #[test]
    fn test_nrf52_uarte_generate_state_enum() {
        let state_defs = nrf52_uarte_state_definitions();
        let register = format_ident!("Nrf52UarteRegisters");
        let store = format_ident!("Nrf52UarteStateEnum");

        let out = generate_state_enum(&state_defs, &register, &store);
        let s = out.to_string();

        assert!(s.contains("Nrf52UarteStateEnum"));
        assert!(s.contains("Nrf52UarteRegisters"));
        assert!(s.contains("Off"));
        assert!(s.contains("ActiveIdle"));
        assert!(s.contains("ActiveRx"));
        assert!(s.contains("ActiveTx"));
        assert!(s.contains("ActiveRxTx"));
        assert!(s.contains("StateEnum"));
        assert!(s.contains("sync_state"));
    }

    #[test]
    fn test_nrf52_uarte_generate_state_emits_impls_for_all_states() {
        let state_defs = nrf52_uarte_state_definitions();
        let register = format_ident!("Nrf52UarteRegisters");
        let store = format_ident!("Nrf52UarteStateEnum");

        let mut full = proc_macro2::TokenStream::new();
        for state_def in &state_defs {
            full.extend(generate_state(state_def, &store, &register));
        }
        let s = full.to_string();

        assert!(s.contains("impl State for Off"));
        assert!(
            s.contains("impl State for Active") && s.contains("RxIdle") && s.contains("TxIdle")
        );
        assert!(s.contains("Transient"));
        assert!(s.contains("impl Reg for Nrf52UarteRegisters"));
        assert!(s.contains("From"));
        assert!(s.contains("TryFrom"));
    }

    fn state_loading_transient() -> StateDefinition {
        let state: State = parse_quote!(Loading);
        StateDefinition {
            state,
            transient: true,
            state_shortname: parse_quote!(Loading),
        }
    }

    fn state_off_def() -> StateDefinition {
        let state: State = parse_quote!(Off);
        StateDefinition {
            state,
            transient: false,
            state_shortname: parse_quote!(Off),
        }
    }

    #[test]
    fn test_generate_state_enum_emits_enum_and_state_enum_impl() {
        let state_defs = vec![state_off_def(), state_loading_transient()];
        let register = format_ident!("PeriphRegisters");
        let store = format_ident!("PeriphStateEnum");

        let out = generate_state_enum(&state_defs, &register, &store);
        let s = out.to_string();

        assert!(s.contains("StateEnum"), "output should implement StateEnum");
        assert!(
            s.contains("PeriphStateEnum"),
            "output should contain state enum name"
        );
        assert!(
            s.contains("PeriphRegisters"),
            "output should contain register name"
        );
        assert!(s.contains("Off"), "output should contain Off variant");
        assert!(
            s.contains("Loading"),
            "output should contain Loading variant"
        );
        assert!(
            s.contains("sync_state"),
            "output should contain sync_state for StateEnum"
        );
    }

    #[test]
    fn test_generate_state_enum_transient_has_sync_state_match_arm() {
        let state_defs = vec![state_off_def(), state_loading_transient()];
        let register = format_ident!("R");
        let store = format_ident!("S");

        let out = generate_state_enum(&state_defs, &register, &store);
        let s = out.to_string();

        // Transient state Loading should get a match arm; sync_state appears in the match
        assert!(
            s.contains("sync_state") && s.contains("Loading"),
            "transient state should appear in match with sync_state: {:?}",
            s
        );
    }

    #[test]
    fn test_generate_state_emits_state_reg_from_tryfrom_impls() {
        let state_def = state_off_def();
        let register = format_ident!("PeriphRegisters");
        let store = format_ident!("PeriphStateEnum");

        let out = generate_state(&state_def, &store, &register);
        let s = out.to_string();

        assert!(
            s.contains("impl State for Off"),
            "output should impl State for state type"
        );
        assert!(
            s.contains("impl Reg for PeriphRegisters"),
            "output should impl Reg for register<state>"
        );
        assert!(s.contains("From"), "output should contain From impl");
        assert!(s.contains("TryFrom"), "output should contain TryFrom impl");
        assert!(s.contains("StateEnum"), "output should reference StateEnum");
        assert!(s.contains("Off"), "output should contain state shortname");
    }

    #[test]
    fn test_form_state_type_signature_simple() {
        let state: State = parse_quote!(Off);
        let ts = state.form_state_type_signature_token();
        assert!(ts.to_string().contains("Off"));
    }

    #[test]
    fn test_form_state_type_signature_with_substates() {
        let state: State = parse_quote!(Active(Tx, Rx));
        let ts = state.form_state_type_signature_token();
        let s = ts.to_string();
        assert!(s.contains("Active"));
        assert!(s.contains("Tx"));
        assert!(s.contains("Rx"));
    }

    /// Regression test: states with `Any` substate must produce valid type args (e.g. `T0`),
    /// not `T0: SubState` in type position (invalid) or "T0: SubState" identifier (panics).
    #[test]
    fn test_form_state_type_signature_with_any_substate() {
        let state: State = parse_quote!(Active(RxIdle, Any));
        let ts = state.form_state_type_signature_token();
        let s = ts.to_string();
        assert!(s.contains("Active"));
        assert!(s.contains("RxIdle"));
        assert!(s.contains("T0"));
        // Bounds go on impl, not in type args
        assert!(
            !s.contains("SubState"),
            "type args should not contain inline bounds"
        );
    }

    #[test]
    fn test_form_impl_generics_token_with_any() {
        let state: State = parse_quote!(Active(RxIdle, Any));
        let generics = state.form_impl_generics_token();
        assert!(
            generics.is_some(),
            "state with Any should have impl generics"
        );
        let s = generics.unwrap().to_string();
        assert!(s.contains("T0"));
        assert!(s.contains("SubState"));
    }

    #[test]
    fn test_form_impl_generics_token_without_any() {
        let state: State = parse_quote!(Active(RxIdle, TxIdle));
        let generics = state.form_impl_generics_token();
        assert!(
            generics.is_none(),
            "state without Any should not have impl generics"
        );
    }

    /// Regression test: generate_state with Any must produce impl<T0: SubState>, not
    /// invalid impl State for Active<T0: SubState, TxIdle> (bounds in type position).
    #[test]
    fn test_generate_state_with_any_uses_impl_generics() {
        let state: State = parse_quote!(Active(RxIdle, Any));
        let state_def = StateDefinition {
            state,
            transient: false,
            state_shortname: parse_quote!(ActiveIdle),
        };
        let register = format_ident!("TestRegisters");
        let store = format_ident!("TestStateEnum");

        let out = generate_state(&state_def, &register, &store);
        let s = out.to_string();

        // Must use impl<T0: SubState>, not impl State for Active<T0: SubState, ...>
        assert!(
            s.contains("impl < T0 : SubState >"),
            "impl block must declare generics: {:?}",
            s
        );
        assert!(
            s.contains("Active < RxIdle , T0 >"),
            "type args must use T0 without inline bound: {:?}",
            s
        );
    }

    // --- Regression tests for E0119/E0277 bugs during Nrf52 UART debugging ---

    /// E0277 regression: StepOff requires Reg. generate_state_conversion_impls_only must emit Reg.
    #[test]
    fn test_generate_state_conversion_impls_only_emits_reg() {
        let state_def = StateDefinition {
            state: parse_quote!(Active(RxIdle, TxIdle)),
            transient: false,
            state_shortname: parse_quote!(ActiveIdle),
        };
        let out = generate_state_conversion_impls_only(
            &state_def,
            &format_ident!("TestStateEnum"),
            &format_ident!("TestRegisters"),
        );
        let s = out.to_string();
        assert!(
            s.contains("impl Reg for TestRegisters"),
            "must emit Reg for StepOff: {:?}",
            s
        );
    }

    /// E0277 regression: must emit From and TryFrom for enum conversions.
    #[test]
    fn test_generate_state_conversion_impls_only_emits_from_tryfrom() {
        let state_def = StateDefinition {
            state: parse_quote!(Active(RxIdle, TxIdle)),
            transient: false,
            state_shortname: parse_quote!(ActiveIdle),
        };
        let out = generate_state_conversion_impls_only(
            &state_def,
            &format_ident!("TestStateEnum"),
            &format_ident!("TestRegisters"),
        );
        let s = out.to_string();
        assert!(s.contains("impl From"), "must emit From: {:?}", s);
        assert!(s.contains("impl TryFrom"), "must emit TryFrom: {:?}", s);
    }

    /// E0119 regression: conversion impls must NOT emit State (generic provides it, would overlap).
    #[test]
    fn test_generate_state_conversion_impls_only_no_state_impl() {
        let state_def = StateDefinition {
            state: parse_quote!(Active(RxIdle, TxIdle)),
            transient: false,
            state_shortname: parse_quote!(ActiveIdle),
        };
        let out = generate_state_conversion_impls_only(
            &state_def,
            &format_ident!("TestStateEnum"),
            &format_ident!("TestRegisters"),
        );
        let s = out.to_string();
        assert!(
            !s.contains("impl State for"),
            "must not emit State (generic provides it, would cause E0119 overlap): {:?}",
            s
        );
    }

    /// E0119 regression: generic impl must NOT emit Reg or TryFrom (would overlap with concrete).
    #[test]
    fn test_generate_state_trait_impls_only_state_only() {
        let state_def = StateDefinition {
            state: parse_quote!(Active(Any, Any)),
            transient: false,
            state_shortname: parse_quote!(ActiveIdle),
        };
        let out = generate_state_trait_impls_only(
            &state_def,
            &format_ident!("TestRegisters"),
            &format_ident!("TestStateEnum"),
        );
        let s = out.to_string();
        assert!(s.contains("impl"), "must emit State impl: {:?}", s);
        assert!(
            !s.contains("impl Reg "),
            "must not emit Reg (would overlap with concrete, E0119): {:?}",
            s
        );
        assert!(
            !s.contains("TryFrom"),
            "must not emit TryFrom (would overlap with concrete, E0119): {:?}",
            s
        );
    }

    /// E0119 regression: verify Nrf52 UART-style output has no duplicate impl patterns.
    /// Concrete states get Reg+From+TryFrom, generic gets State only.
    #[test]
    fn test_nrf52_uarte_impl_split_no_overlaps() {
        let state_defs = nrf52_uarte_state_definitions();
        let register = format_ident!("Nrf52UarteRegisters");
        let store = format_ident!("Nrf52UarteStateEnum");

        let mut full = proc_macro2::TokenStream::new();
        for state_def in &state_defs {
            let state = &state_def.state;
            let is_concrete = !state.contains_any();
            let has_substates = !state.substates.is_empty();

            if is_concrete {
                if has_substates {
                    full.extend(generate_state_conversion_impls_only(
                        state_def, &store, &register,
                    ));
                } else {
                    full.extend(generate_state(state_def, &store, &register));
                }
            }
        }
        // Add generic State impl (from Any,Any case)
        let any_any_def = StateDefinition {
            state: parse_quote!(Active(Any, Any)),
            transient: false,
            state_shortname: parse_quote!(ActiveIdle),
        };
        full.extend(generate_state_trait_impls_only(
            &any_any_def,
            &register,
            &store,
        ));

        let s = full.to_string();
        // Should have exactly one "impl Reg for Nrf52UarteRegisters" per concrete state
        let reg_count = s.matches("impl Reg for Nrf52UarteRegisters").count();
        assert_eq!(
            reg_count, 5,
            "expect 5 Reg impls (Off + 4 Active variants), got {}: {:?}",
            reg_count, s
        );
        // Should have exactly one "impl TryFrom" per concrete state
        let tryfrom_count = s.matches("impl TryFrom").count();
        assert_eq!(
            tryfrom_count, 5,
            "expect 5 TryFrom impls, got {}: {:?}",
            tryfrom_count, s
        );
        // Should have exactly one generic State impl (for Active<T0, T1>)
        assert!(
            s.contains("impl < T0 : SubState , T1 : SubState > State for Active < T0 , T1 >"),
            "must have generic State impl: {:?}",
            s
        );
    }
}
