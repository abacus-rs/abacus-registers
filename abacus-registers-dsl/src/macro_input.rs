use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::{
    bracketed,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Ident,
};

use std::collections::{hash_set::HashSet, HashMap};

use crate::generating;
use crate::parsing::{State, StateDefinition};

mod custom_keywords {
    syn::custom_keyword!(peripheral_name);
    syn::custom_keyword!(registers);
    syn::custom_keyword!(states);
    syn::custom_keyword!(register_base_addr);
}

pub struct MacroInput {
    pub peripheral_name: String,
    pub state_definitions: Vec<StateDefinition>,
}

impl MacroInput {
    pub fn generate_states(
        &self,
        register_name: &Ident,
        state_enum_name: &Ident,
    ) -> (proc_macro2::TokenStream, HashMap<String, State>) {
        let mut output = proc_macro2::TokenStream::new();

        // The other state hashes include the substates. This is strictly
        // looking at the State name
        let mut strict_state_hash = HashSet::new();

        let mut created_states: HashSet<syn::Ident> = HashSet::new();

        let mut unique_substates = HashSet::new();
        unique_substates.insert(format_ident!("Any"));

        let mut state_hash = HashSet::new();

        let mut state_map = HashMap::new();
        for state_def in &self.state_definitions {
            let state = state_def.state.clone();
            if !strict_state_hash.contains(state.state_name.to_string().as_str()) {
                if state.substates.is_empty() {
                    output.extend(quote! {});
                } else if state.substates.len() == 1 {
                    output.extend(quote! {})
                } else if state.substates.len() == 2 {
                    output.extend(quote! {});
                } else {
                    unimplemented!("Only 2 substates are supported.");
                }

                strict_state_hash.insert(state.state_name.to_string());
            }

            // State hash map used for name mapping later.
            state_map.insert(state.form_state_marker_type().to_string(), state.clone());

            let mut state = state.clone();
            let original_substates = state.substates.clone();

            // We need to generate each substate as:
            // 1. The specified state.
            // 2. As potentially an any state.
            // 3. As potentially all being any states.
            // TODO: We need to add logic for if there are 3 substates (e.g. <Any, Any, Tx>)
            for iter in 0..(&state.substates.len() + 2) {
                let mut _any_positions: Option<Vec<(usize, Ident)>> = None;

                // Case (1)
                state.substates = original_substates.clone();

                // Case (2)
                if iter < state.substates.len() {
                    _any_positions = Some(vec![(iter, state.substates[iter].clone())]);
                    state.substates[iter] = format_ident!("Any");
                }

                // Case (3)
                if iter == state.substates.len() {
                    // Update substates to all be "Any" and record positions with prior ident value
                    let mut vec: Vec<(usize, Ident)> = Vec::new();
                    for (pos, substate) in state.substates.iter().enumerate() {
                        vec.push((pos, substate.clone()));
                    }

                    _any_positions = Some(vec);
                    state
                        .substates
                        .iter_mut()
                        .for_each(|substate| *substate = format_ident!("Any"));
                }

                let state_ident = state.state_name.clone();

                for substate in &state.substates {
                    unique_substates.insert(substate.clone());
                }

                let struct_name = if state.substates.is_empty() {
                    quote! {#state_ident}
                } else {
                    let generic_params = state.substates.iter().enumerate().map(|(index, _)| {
                        let entry = format!("T{}", index);
                        let generic = syn::Ident::new(&entry, Span::call_site());

                        quote! {
                            #generic: SubState
                        }
                    });

                    quote! {
                        #state_ident<#(#generic_params),*>
                    }
                };

                let fields = state.substates.iter().enumerate().map(|(index, _)| {
                    let field_name = format!("associated_{}", index);
                    let generic_name = format!("T{}", index);

                    let generic = syn::Ident::new(&generic_name, Span::call_site());
                    let field = syn::Ident::new(&field_name, Span::call_site());

                    quote! {
                        #field: PhantomData<#generic>
                    }
                });

                // To avoid duplicate implementations for a type, check if it has already been used.
                let concrete_state_str = state.form_concrete_state_type().to_string();
                if state_hash.contains(&concrete_state_str) {
                    // if any_positions.is_some() {
                    //     output.extend(state.generate_state(register_name, store_name, &struct_name, all_states_vec.clone(), any_positions, true));
                    // }
                    continue; // Skip creating this state, as it has already been created.
                } else {
                    state_hash.insert(concrete_state_str.clone());
                }

                if !created_states.contains(&state.state_name) {
                    output.extend(quote! {
                        pub struct #struct_name {
                            #(#fields),*
                        }
                    });

                    created_states.insert(state.state_name.clone());
                }

                // Generate State/Reg/From/TryFrom impls. Avoid overlapping impls (E0119):
                // - Concrete states without substates (e.g. Off): full impls
                // - Concrete states with substates (e.g. Active<RxIdle, TxIdle>): From/TryFrom
                //   only (State/Reg provided by generic impl from (Any, Any))
                // - Fully generic (Any, Any): State and Reg only
                // - Partially generic (Any, TxIdle), (RxIdle, Any): skip
                let is_concrete = !state.contains_any();
                let is_fully_generic = state.substates.iter().all(|s| s.to_string() == "Any");
                let has_substates = !state.substates.is_empty();
                if is_concrete {
                    let synthetic_state_def = StateDefinition {
                        state: state.clone(),
                        transient: state_def.transient,
                        state_shortname: state_def.state_shortname.clone(),
                    };
                    if has_substates {
                        output.extend(generating::generate_state_conversion_impls_only(
                            &synthetic_state_def,
                            state_enum_name,
                            register_name,
                        ));
                    } else {
                        output.extend(generating::generate_state(
                            &synthetic_state_def,
                            state_enum_name,
                            register_name,
                        ));
                    }
                } else if is_fully_generic {
                    let synthetic_state_def = StateDefinition {
                        state: state.clone(),
                        transient: state_def.transient,
                        state_shortname: state_def.state_shortname.clone(),
                    };
                    output.extend(generating::generate_state_trait_impls_only(
                        &synthetic_state_def,
                        register_name,
                        state_enum_name,
                    ));
                }
            }
        }

        // create substates
        for substate in unique_substates {
            let substate_ident = format_ident!("{}", substate);

            output.extend(quote! {
                pub struct #substate_ident {}

                impl SubState for #substate_ident {}
            });
        }

        (output, state_map)
    }
}

impl Parse for MacroInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let _: custom_keywords::peripheral_name = input.parse().map_err(|_| {
            syn::Error::new(
                input.span(),
                "Abacus Macro - Expected 'peripheral_name' keyword",
            )
        })?;
        let _: syn::Token![=] = input.parse().map_err(|_| {
            syn::Error::new(
                input.span(),
                "Abacus Macro - Expected '=' after peripheral_name",
            )
        })?;
        let peripheral_name: syn::LitStr = input.parse().map_err(|_| {
            syn::Error::new(
                input.span(),
                "Abacus Macro - Expected string literal for peripheral_name",
            )
        })?;
        let _: syn::Token![,] = input.parse().map_err(|_| {
            syn::Error::new(
                input.span(),
                "Abacus Macro - Expected ',' after peripheral_name value",
            )
        })?;

        let _: custom_keywords::register_base_addr = input.parse().map_err(|_| {
            syn::Error::new(
                input.span(),
                "Abacus Macro - Expected 'register_base_addr' keyword",
            )
        })?;
        let _: syn::Token![=] = input.parse().map_err(|_| {
            syn::Error::new(
                input.span(),
                "Abacus Macro - Expected '=' after register_base_addr",
            )
        })?;
        let _base_addr: syn::LitInt = input.parse().map_err(|_| {
            syn::Error::new(
                input.span(),
                "Abacus Macro - Expected integer literal for register_base_addr",
            )
        })?;
        let _: syn::Token![,] = input.parse().map_err(|_| {
            syn::Error::new(
                input.span(),
                "Abacus Macro - Expected ',' after register_base_addr value",
            )
        })?;

        let _: custom_keywords::states = input.parse().map_err(|_| {
            syn::Error::new(input.span(), "Abacus Macro - Expected 'states' keyword")
        })?;
        let _: syn::Token![=] = input.parse().map_err(|_| {
            syn::Error::new(input.span(), "Abacus Macro - Expected '=' after states")
        })?;
        let states_content;
        let _: syn::token::Bracket = bracketed!(states_content in input);
        let state_definitions: Punctuated<StateDefinition, syn::Token![,]> = states_content
            .parse_terminated(StateDefinition::parse, syn::Token![,])
            .map_err(|err| {
                syn::Error::new(
                    states_content.span(),
                    format!(
                        "Abacus Macro - Error parsing state definitions list: {}",
                        err
                    ),
                )
            })?;

        Ok(MacroInput {
            peripheral_name: peripheral_name.value(),
            state_definitions: state_definitions.into_iter().collect(),
        })
    }
}
