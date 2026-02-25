use crate::parsing::{State, StateDefinition};
use quote::{format_ident, quote};
use syn::Ident;

/// Struct containing all generated TokenStreams
/// that are created for a given specified state.
struct GeneratedTotalState {}

struct GeneratedStateType {
    generic_parameter_list: Option<Vec<Ident>>,
    type_signature: proc_macro2::TokenStream,
}

struct GeneratedRegister {}

type GeneratedStateEnum = proc_macro2::TokenStream;

impl GeneratedStateType {
    /// Create a new GeneratedStateType from a State.
    ///
    /// Consumes parsed State and forms the Ident that serves as
    /// the corresponding type signature.
    fn new(mut state: State) -> Self {
        let state_ident = &state.state_name;
        let mut generics_list = Vec::new();

        let type_signature = if state.substates.is_empty() {
            quote! { #state_ident }
        } else {
            // We need to replace the `Any` keyword with a generic
            // that is incremented for each (e.g. State<SubState, Any, Any>
            // must become State<SubState, T0: SubState, T1: SubState>)
            let args = if state.contains_any() {
                let mut generic_count = 0;
                for substate in state.substates.iter_mut() {
                    if substate.to_string() == "Any" {
                        let generic_ident = format_ident!("T{}", generic_count.to_string());
                        generics_list.push(generic_ident.clone());
                        *substate = format_ident!("{}: SubState", generic_ident);
                        generic_count += 1;
                    }
                }
                state.substates.iter()
            } else {
                state.substates.iter()
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
    fn contains_any(&self) -> bool {
        self.substates
            .iter()
            .any(|substate| substate.to_string() == "Any")
    }

    /// Produces the type signature token stream for this state (e.g. `Off`, `Active<Tx, Rx>`,
    /// or `Active<T0: SubState, T1: SubState>` when substates contain `Any`).
    pub fn form_state_type_signature_token(&self) -> proc_macro2::TokenStream {
        GeneratedStateType::new(self.clone()).type_signature
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
        // Only match / call `reg.sync_state` for transient states.
        // `sync_state` is only defined for transient states.
        if state_def.transient {
            quote! { #store_name::#variant_name(reg) => reg.sync_state() }
        } else {
            quote! {}
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
                    x => x
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

    result.extend(quote! {
            impl State for #state_type_signature_ident {
            }

            impl Reg for #register_name<#state_type_signature_ident> {
                type StateEnum = #state_enum_name;

            }

    });

    result.extend(quote! {
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

    result
}

/*fn map_any_generics(state: &State) -> State {
    // An `Any` substate means any state is valid. To mock up this behavior in the
    // type system, we must replace the Any substate with a generic type.
    let map_any = |mut state: State, generic_seed: String| {
        // For any substate that is Any, replace with generic T.

        let form_generic = |generic: Ident| {
            quote!(
                #generic: SubState
            )
        };

        // These substates may be different, so we need to make distinct
        // generics.
        let mut count = 0;
        for substate in state.substates.iter_mut() {
            if substate.to_string() == "Any" {
                *substate = format_ident!("{}{}", generic_seed, count.to_string());
                count += 1;
            }
        }

        // create comma separated list T0, T1, ..., T(n) for the number
        // count
        let generic_params = (0..count).map(|index| {
            let generic = format_ident!("{}{}", generic_seed, index.to_string());
            generic
        });

        let generic_params_constrained: Vec<_> = (0..count)
            .map(|index| {
                let generic = format_ident!("{}{}", generic_seed, index.to_string());
                form_generic(generic)
            })
            .collect();

        let generic_tokens = quote! {
            #(#generic_params),*
        };

        (
            state.form_concrete_state_type(),
            generic_tokens,
            generic_params_constrained,
        )
    }
}*/

/*fn generate_abacus_register(register_attribute: RegisterAttributes, generated_transitions: HashSet<Ident>) -> (proc_macro2::TokenStream, HashSet<Ident>) {
        let register_bitwidth = register_attribute.register_bitwidth.clone();
        let register_shortname = register_attribute.register_shortname.clone();
        let validstate = register_attribute
            .valid_states
            .first()
            .expect("generate reg op bindings")
            .form_state_marker_type();


        // FIXME: This only accounts for the first state. We need to account for all states.in the
        // valid states. StateChangeRegister currently does this, but all register types should.
        match &self.register_type {
            RegisterType::ReadOnly => {
                if is_anytype {
                    let (state_ident, generic_tokens, generic_tokens_constrained) =
                        map_any(self.valid_states.first().unwrap().clone(), format! {"T"});
                    quote! {
                        impl <#generic_tokens> ReadOnlyRegister<#register_bitwidth, #register_shortname, #state_ident>
                        where
                            #state_ident: State,
                            #(#generic_tokens_constrained),*
                        {
                            pub fn get(&self) -> #register_bitwidth {
                                self.reg.get()
                            }

                            pub fn read(&self, field: Field<#register_bitwidth, #register_shortname>) -> #register_bitwidth {
                                self.reg.read(field)
                            }
                        }
                    }
                } else {
                    quote! {
                        impl ReadOnlyRegister<#register_bitwidth, #register_shortname, #validstate> {
                            pub fn get(&self) -> #register_bitwidth {
                                self.reg.get()
                            }

                            pub fn read(&self, field: Field<#register_bitwidth, #register_shortname>) -> #register_bitwidth {
                                self.reg.read(field)
                            }
                        }
                    }
                }
            }
    match register_attribute.register_type {
        ReadOnly => {

        }
    }
    unimplemented!()
}*/

/*

    fn generate_register_op_bindings(
        &self,
        register_name: &Ident,
    ) -> proc_macro2::TokenStream {
        let register_bitwidth = self.register_bitwidth.clone();
        let register_shortname = self.register_shortname.clone();
        let validstate = self
            .valid_states
            .first()
            .expect("generate reg op bindings")
            .form_state_marker_type();

        // Determine if this state contains an Any substate.
        let is_anytype = self
            .valid_states
            .first()
            .unwrap()
            .substates
            .iter()
            .any(|substate| substate.to_string() == "Any");

        // An `Any` substate means any state is valid. To mock up this behavior in the
        // type system, we must replace the Any substate with a generic type.
        let map_any = |mut state: State, generic_seed: String| {
            // For any substate that is Any, replace with generic T.

            let form_generic = |generic: Ident| {
                quote!(
                    #generic: SubState
                )
            };

            // These substates may be different, so we need to make distinct
            // generics.
            let mut count = 0;
            for substate in state.substates.iter_mut() {
                if substate.to_string() == "Any" {
                    *substate = format_ident!("{}{}", generic_seed, count.to_string());
                    count += 1;
                }
            }

            // create comma separated list T0, T1, ..., T(n) for the number
            // count
            let generic_params = (0..count).map(|index| {
                let generic = format_ident!("{}{}", generic_seed, index.to_string());
                generic
            });

            let generic_params_constrained: Vec<_> = (0..count)
                .map(|index| {
                    let generic = format_ident!("{}{}", generic_seed, index.to_string());
                    form_generic(generic)
                })
                .collect();

            let generic_tokens = quote! {
                #(#generic_params),*
            };

            (
                state.form_concrete_state_type(),
                generic_tokens,
                generic_params_constrained,
            )
        };

        // FIXME: This only accounts for the first state. We need to account for all states.in the
        // valid states. StateChangeRegister currently does this, but all register types should.
        match &self.register_type {
            RegisterType::ReadOnly => {
                if is_anytype {
                    let (state_ident, generic_tokens, generic_tokens_constrained) =
                        map_any(self.valid_states.first().unwrap().clone(), format! {"T"});
                    quote! {
                        impl <#generic_tokens> ReadOnlyRegister<#register_bitwidth, #register_shortname, #state_ident>
                        where
                            #state_ident: State,
                            #(#generic_tokens_constrained),*
                        {
                            pub fn get(&self) -> #register_bitwidth {
                                self.reg.get()
                            }

                            pub fn read(&self, field: Field<#register_bitwidth, #register_shortname>) -> #register_bitwidth {
                                self.reg.read(field)
                            }
                        }
                    }
                } else {
                    quote! {
                        impl ReadOnlyRegister<#register_bitwidth, #register_shortname, #validstate> {
                            pub fn get(&self) -> #register_bitwidth {
                                self.reg.get()
                            }

                            pub fn read(&self, field: Field<#register_bitwidth, #register_shortname>) -> #register_bitwidth {
                                self.reg.read(field)
                            }
                        }
                    }
                }
            }
            RegisterType::WriteOnly => {
                if is_anytype {
                    let (state_ident, generic_tokens, generic_tokens_constrained) =
                        map_any(self.valid_states.first().unwrap().clone(), format! {"T"});
                    quote! {
                        impl <#generic_tokens> WriteOnlyRegister<#register_bitwidth, #register_shortname, #state_ident>
                        where
                            #state_ident: State,
                            #(#generic_tokens_constrained),*
                        {
                            pub fn set(&self, value: #register_bitwidth) {
                                self.reg.set(value)
                            }

                            pub fn write(&self, value: FieldValue<#register_bitwidth, #register_shortname>) {
                                self.reg.write(value)
                            }
                        }
                    }
                } else {
                    quote! {
                        impl WriteOnlyRegister<#register_bitwidth, #register_shortname, #validstate> {
                            pub fn set(&self, value: #register_bitwidth) {
                                self.reg.set(value)
                            }

                            pub fn write(&self, value: FieldValue<#register_bitwidth, #register_shortname>) {
                                self.reg.write(value)
                            }

                            pub fn modify_no_read(&self,
                                original: LocalRegisterCopy<#register_bitwidth, #register_shortname>,
                                value: FieldValue<#register_bitwidth, #register_shortname>) {
                                    self.reg.modify_no_read(original, value)
                            }
                        }
                    }
                }
            }
            RegisterType::ReadWrite => {
                if is_anytype {
                    let (state_ident, generic_tokens, generic_tokens_constrained) =
                        map_any(self.valid_states.first().unwrap().clone(), format! {"T"});
                    quote! {
                        impl <#generic_tokens> ReadWriteRegister<#register_bitwidth, #register_shortname, #state_ident>
                        where
                            #state_ident: State,
                            #(#generic_tokens_constrained),*
                        {
                            pub fn get(&self) -> #register_bitwidth {
                                self.reg.get()
                            }
                            pub fn set(&self, value: #register_bitwidth) {
                                self.reg.set(value)
                            }

                            pub fn write(&self, value: FieldValue<#register_bitwidth, #register_shortname>) {
                                self.reg.write(value)
                            }

                            pub fn is_set(&self, field: Field<#register_bitwidth, #register_shortname>) -> bool {
                                self.reg.is_set(field)
                            }

                            pub fn modify(&self, value: FieldValue<#register_bitwidth, #register_shortname>) {
                                self.reg.modify(value)
                            }

                            pub fn modify_no_read(&self,
                                original: LocalRegisterCopy<#register_bitwidth, #register_shortname>,
                                value: FieldValue<#register_bitwidth, #register_shortname>) {
                                    self.reg.modify_no_read(original, value)
                            }

                            pub fn read(&self, field: Field<#register_bitwidth, #register_shortname>) -> #register_bitwidth {
                                self.reg.read(field)
                            }
                        }
                    }
                } else {
                    quote! {
                        impl ReadWriteRegister<#register_bitwidth, #register_shortname, #validstate> {
                            pub fn get(&self) -> #register_bitwidth {
                                self.reg.get()
                            }
                            pub fn set(&self, value: #register_bitwidth) {
                                self.reg.set(value)
                            }

                            pub fn write(&self, value: FieldValue<#register_bitwidth, #register_shortname>) {
                                self.reg.write(value)
                            }
                            pub fn is_set(&self, field: Field<#register_bitwidth, #register_shortname>) -> bool {
                                self.reg.is_set(field)
                            }

                            pub fn modify(&self, value: FieldValue<#register_bitwidth, #register_shortname>) {
                                self.reg.modify(value)
                            }

                            pub fn modify_no_read(&self,
                                original: LocalRegisterCopy<#register_bitwidth, #register_shortname>,
                                value: FieldValue<#register_bitwidth, #register_shortname>) {
                                    self.reg.modify_no_read(original, value)
                            }

                            pub fn read(&self, field: Field<#register_bitwidth, #register_shortname>) -> #register_bitwidth {
                                self.reg.read(field)
                            }
                        }
                    }
                }
            }
            RegisterType::StateChangeRW => {
                if is_anytype {
                    let (state_ident, generic_tokens, generic_tokens_constrained) =
                        map_any(self.valid_states.first().unwrap().clone(), format! {"T"});
                    quote! {
                        impl <#generic_tokens> StateChangeRegister<#register_bitwidth, #register_shortname, #state_ident>
                        where
                            #state_ident: State,
                            #(#generic_tokens_constrained),*
                        {
                            pub fn get(&self) -> #register_bitwidth {
                                self.reg.get()
                            }
                            pub fn set(&self, value: #register_bitwidth) {
                                self.reg.set(value)
                            }

                            pub fn write(&self, value: FieldValue<#register_bitwidth, #register_shortname>) {
                                self.reg.write(value)
                            }

                            pub fn is_set(&self, field: Field<#register_bitwidth, #register_shortname>) -> bool {
                                self.reg.is_set(field)
                            }

                            pub fn modify(&self, value: FieldValue<#register_bitwidth, #register_shortname>) {
                                self.reg.modify(value)
                            }

                            pub fn modify_no_read(&self,
                                original: LocalRegisterCopy<#register_bitwidth, #register_shortname>,
                                value: FieldValue<#register_bitwidth, #register_shortname>) {
                                    self.reg.modify_no_read(original, value)
                            }

                            pub fn read(&self, field: Field<#register_bitwidth, #register_shortname>) -> #register_bitwidth {
                                self.reg.read(field)
                            }
                        }
                    }
                } else {
                    quote! {
                        impl StateChangeRegister<#register_bitwidth, #register_shortname, #validstate> {
                            pub fn get(&self) -> #register_bitwidth {
                                self.reg.get()
                            }
                            pub fn set(&self, value: #register_bitwidth) {
                                self.reg.set(value)
                            }

                            pub fn write(&self, value: FieldValue<#register_bitwidth, #register_shortname>) {
                                self.reg.write(value)
                            }
                            pub fn is_set(&self, field: Field<#register_bitwidth, #register_shortname>) -> bool {
                                self.reg.is_set(field)
                            }

                            pub fn modify(&self, value: FieldValue<#register_bitwidth, #register_shortname>) {
                                self.reg.modify(value)
                            }

                            pub fn modify_no_read(&self,
                                original: LocalRegisterCopy<#register_bitwidth, #register_shortname>,
                                value: FieldValue<#register_bitwidth, #register_shortname>) {
                                    self.reg.modify_no_read(original, value)
                            }

                            pub fn read(&self, field: Field<#register_bitwidth, #register_shortname>) -> #register_bitwidth {
                                self.reg.read(field)
                            }
                        }
                    }
                }
            }
            RegisterType::StateChange(state, instruction) => {
                // for Punctuated<Path, Comma> form Path1 + Path2 + Path3
                let instruct_ident = instruction.iter().map(|instr| {
                    quote! {
                        #instr
                    }
                });

                let instruction = quote! {
                    #(#instruct_ident)+*
                };


                let mut state_change_output = proc_macro2::TokenStream::new();

                let reg_field_name = self.name.clone();
                let state_shortname = &state.state_name;
                let trait_name = format_ident!("Step{}", state_shortname);

                let to_state_fn_name =
                    format_ident!("into_{}", state_shortname.to_string().to_lowercase());

                if is_anytype {
                    // Create copy of state to change
                    let state = state.clone();
                    let (to_state, to_state_generics, to_state_generics_constrained) =
                        map_any(state, "T".to_string());

                    // TODO: This is just a marker that we have an issue here. We will need to update
                    // <impl T: SubState> to have more generics than T / F for more complex peripherals.

                    let carrot_block = if to_state_generics_constrained.is_empty() {
                        quote! {S}
                    } else {
                        quote! {#(#to_state_generics_constrained)*, S}
                    };

                    state_change_output.extend(quote! {
                        trait #trait_name<#carrot_block>: Sized
                        where
                            #to_state: State,
                            S: State,
                            #register_name<#to_state>: Reg,
                            #register_name<S>: Reg,
                            #(#to_state_generics_constrained),*
                        {
                            fn #to_state_fn_name(
                                self,
                            ) -> #register_name<#to_state>;
                        }
                    });

                    for state in &self.valid_states {
                        let (from_state, _from_state_generics, from_state_generics_constrained) =
                            map_any(state.clone(), "T".to_string());

                        let constraints = merge_constraint_vec(
                            to_state_generics_constrained.clone(),
                            from_state_generics_constrained,
                        );

                        let carrot_block = if to_state_generics.to_string() == "" {
                            quote! {#from_state}
                        } else {
                            quote! {#to_state_generics, #from_state}
                        };

                        state_change_output.extend(quote!{
                            impl <#(#constraints),*> #trait_name<#carrot_block> for #register_name<#from_state>
                            where
                                #to_state: State,
                                #from_state: State,
                                #register_name<#to_state>: Reg,
                                #register_name<#from_state>: Reg,
                                #(#constraints),*
                            {
                                fn #to_state_fn_name(
                                    self,
                                ) -> #register_name<#to_state> {
                                    self.#reg_field_name.reg.modify(#instruction);

                                    unsafe {
                                        transmute::<
                                            #register_name<#from_state>,
                                            #register_name<#to_state>
                                        >(self)
                                    }
                                }
                            }
                        })
                    }

                    state_change_output
                } else {
                    let to_state = state.form_concrete_state_type();

                    state_change_output.extend(quote! {
                        trait #trait_name<S: State>: Sized
                        where
                            #register_name<S>: Reg
                        {
                            fn #to_state_fn_name(
                                self,
                            ) -> #register_name<#to_state>;
                        }
                    });

                    for state in &self.valid_states {
                        let from_state = state.form_concrete_state_type();

                        state_change_output.extend(quote! {
                            impl #trait_name<#from_state> for #register_name<#from_state> {
                                fn #to_state_fn_name(
                                    self,
                                ) -> #register_name<#to_state> {
                                    self.#reg_field_name.reg.modify(#instruction);

                                    unsafe {
                                        transmute::<
                                            #register_name<#from_state>,
                                            #register_name<#to_state>
                                        >(self)
                                    }
                                }
                            }
                        });
                    }

                    state_change_output
                }
            }
        }
    }
}

*/

// Reference samples in code_gen/src/ (inspiration only, not loaded by tests):
// - sample_code_gen_1: UART (Nrf52Uarte) — input + old generated output.
// - sample_code_gen_2: Nrf5xTemp — input + old generated output.

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
                StateDefinition { state, transient: false, state_shortname: parse_quote!(Off) }
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
        assert!(s.contains("impl State for Active") && s.contains("RxIdle") && s.contains("TxIdle"));
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
        assert!(s.contains("PeriphStateEnum"), "output should contain state enum name");
        assert!(s.contains("PeriphRegisters"), "output should contain register name");
        assert!(s.contains("Off"), "output should contain Off variant");
        assert!(s.contains("Loading"), "output should contain Loading variant");
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

        assert!(s.contains("impl State for Off"), "output should impl State for state type");
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
}
