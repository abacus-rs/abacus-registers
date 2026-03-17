use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{punctuated::Punctuated, Ident};

use crate::parsing::{RegisterType, State};
use std::collections::HashSet;

pub struct Register {
    pub name: Ident,
    pub valid_states: Punctuated<State, syn::Token![,]>,
    pub register_shortname: syn::GenericArgument,
    pub register_type: RegisterType,
    pub register_bitwidth: Ident,
}

impl Register {
    /// Create passthrough for valid tock-register operations for each
    /// AbacusRegister type.
    pub fn generate_register_op_bindings(&self, register_name: &Ident) -> proc_macro2::TokenStream {
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
            RegisterType::StateChange(state, instruction, shortname) => {
                // for Punctuated<Path, Comma> form Path1 + Path2 + Path3
                let instruct_ident = instruction.iter().map(|instr| {
                    quote! {
                        #instr
                    }
                });

                let instruction_ts: proc_macro2::TokenStream = quote! {
                    #(#instruct_ident)+*
                };

                let mut state_change_output = proc_macro2::TokenStream::new();

                let reg_field_name = self.name.clone();
                // Use shortname (e.g. transmit, receive) when provided, else state name (e.g. On)
                let method_shortname = shortname.as_ref().unwrap_or(&state.state_name);
                let trait_name = format_ident!("Step{}", method_shortname);

                let to_state_fn_name =
                    format_ident!("into_{}", method_shortname.to_string().to_lowercase());

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
                                    self.#reg_field_name.reg.modify(#instruction_ts);

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
                                    self.#reg_field_name.reg.modify(#instruction_ts);

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

pub fn merge_constraint_vec(vec1: Vec<TokenStream>, vec2: Vec<TokenStream>) -> Vec<TokenStream> {
    let mut added: HashSet<_> = HashSet::new();
    for item in vec1.clone() {
        added.insert(item.clone().to_string());
    }

    let mut output = vec1.clone();
    for item in vec2 {
        if !added.contains(item.clone().to_string().as_str()) {
            output.push(item.clone());
            added.insert(item.clone().to_string());
        }
    }
    output
}
