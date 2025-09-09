use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::{
    bracketed,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    DeriveInput, Field, FieldMutability, Fields, FieldsUnnamed, Ident, ItemFn, PathArguments, Type,
    Variant, Visibility,
};

use std::{
    collections::{hash_set::HashSet, HashMap},
};

#[derive(Clone, Eq, PartialEq, Hash)]
struct State {
    ident: syn::Ident,
    shortname: syn::Ident,
    substates: Punctuated<syn::Ident, syn::Token![,]>,
    transitions: Punctuated<State, syn::Token![,]>,
}

impl State {
    fn form_concrete_state_type(&self) -> proc_macro2::TokenStream {
        let state_ident = self.ident.clone();

        if self.substates.is_empty() {
            quote! {
                #state_ident
            }
        } else {
            let args = self.substates.iter().map(|arg| {
                quote! {
                    #arg
                }
            });

            let args_ident = quote! {#(#args),*};
            quote! {
                #state_ident<#args_ident>
            }
        }
    }

    fn generate_state(
        &self,
        register_name: &Ident,
        store_name: &Ident,
        struct_name: &proc_macro2::TokenStream,
        merge_body: Vec<&State>,
        any_positions: Option<Vec<(usize, Ident)>>,
    ) -> proc_macro2::TokenStream {
        let mut result = proc_macro2::TokenStream::new();

        let state_ident = self.ident.clone();
        let struct_shortname = self.shortname.clone();
        // Form struct name / generics in carrots
        let concrete_type = self.form_concrete_state_type();

        // Form full struct using formed name
        if self.substates.is_empty() {
            result.extend(quote! {
                impl State for #struct_name {
                    type Reg = #register_name<#state_ident>;
                    type StateEnum = #store_name;
                }

                impl Reg for #register_name<#struct_name> {
                    type StateEnum = #store_name;

                }

                impl From<#register_name<#struct_name>> for #store_name {
                    fn from(reg: #register_name<#struct_name>) -> Self {
                        #store_name::#state_ident(reg)
                    }
                }

                impl TryFrom<#store_name> for #register_name<#struct_name> {
                    type Error = #store_name;
                    fn try_from(store: #store_name) -> Result<Self, Self::Error> {
                        match store {
                            #store_name::#state_ident(reg) => Ok(reg),
                            _ => Err(store),
                        }
                    }
                }
            });
        } else {
            result.extend(quote! {
                impl State for #concrete_type {
                    type Reg = #register_name<#concrete_type>;
                    type StateEnum = #store_name;
                }

                impl Reg for #register_name<#concrete_type> {
                    type StateEnum = #store_name;
                }

            });

            if any_positions.is_some() {
                let states_vec = merge_body.clone();
                let merge_body = merge_body.iter().map(|state| {
                    let enum_variant = state.shortname.clone();

                    if is_mergeable(self, state) {
                        quote! {
                            #store_name::#enum_variant(reg) => Ok(reg.merge(self).into())
                        }
                    } else {
                        quote! {
                            #store_name::#enum_variant(reg) => Err(#store_name::#enum_variant(reg))
                        }
                    }
                });

                // TODO: Check that we got the merge logic correct here.
                let try_from_body = states_vec.iter().map(|state| {
                    let enum_variant = state.shortname.clone();
                    if is_valid_into(self, state) {
                        quote! {
                            #store_name::#enum_variant(reg) => Ok(
                                unsafe { transmute::<_, Self>(reg) }
                            ),
                        }
                    } else {
                        quote! {
                            #store_name::#enum_variant(reg) => Err(#store_name::#enum_variant(reg)),
                        }
                    }
                });

                result.extend(quote! {
                        impl Merge<#store_name> for #register_name<#concrete_type> {
                            type Output = Result<#store_name, #store_name>;

                            fn merge(self, other: #store_name) -> Self::Output {
                                match other {
                                    #(#merge_body),*
                                }
                            }
                        }

                        impl AnyReg for #register_name<#concrete_type> {}

                        /*impl From<#register_name<#concrete_type>> for #store_name {
                            fn from(reg: #register_name<#concrete_type>) -> Self {
                                unimplemented!();
                            }
                        }*/

                        impl TryFrom<#store_name> for #register_name<#concrete_type> {
                            type Error = #store_name;
                            fn try_from(store: #store_name) -> Result<Self, Self::Error> {
                                match store {
                                    #(#try_from_body)*
                                }
                            }
                        }
                });
            } else {
                result.extend(quote! {
                    impl From<#register_name<#concrete_type>> for #store_name {
                      fn from(reg: #register_name<#concrete_type>) -> Self {
                          #store_name::#struct_shortname(reg)
                      }
                    }

                    impl TryFrom<#store_name> for #register_name<#concrete_type> {
                        type Error = #store_name;
                        fn try_from(store: #store_name) -> Result<Self, Self::Error> {
                            match store {
                                #store_name::#struct_shortname(reg) => Ok(reg),
                                _ => Err(store),
                            }
                        }
                    }
                });
            }
        }

        result
    }
}

impl Parse for State {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let state: Ident = input.parse().expect("state_parse 0");

        let substates = if input.peek(syn::token::Paren) {
            let content;
            let _: syn::token::Paren = syn::parenthesized!(content in input);
            let substates: Punctuated<Ident, syn::Token![,]> = content
                .parse_terminated(syn::Ident::parse, syn::Token![,])
                .expect("state_parse 1");
            Some(substates)
        } else {
            None
        }
        .unwrap_or_else(|| Punctuated::new());

        let (transitions, shortname) = if input.peek(syn::Token![=>]) {
            input.parse::<syn::Token![=>]>().expect("state_parse 2");
            let content;
            let _: syn::token::Bracket = bracketed!(content in input);
            let transitions: Punctuated<State, syn::Token![,]> = content
                .parse_terminated(State::parse, syn::Token![,])
                .expect("state_parse 3");

            let content;

            if input.peek(syn::token::Brace) {
                let _: syn::token::Brace = syn::braced!(content in input);
                let shortname = content.parse().map_or_else(|_| state.clone(), |x| x);
                (transitions, shortname)
            } else {
                (transitions, state.clone())
            }
        } else {
            (Punctuated::new(), state.clone())
        };

        Ok(State {
            ident: state,
            shortname,
            substates,
            transitions,
        })
    }
}

mod custom_keywords {
    syn::custom_keyword!(peripheral_name);
    syn::custom_keyword!(registers);
    syn::custom_keyword!(states);
    syn::custom_keyword!(register_base_addr);
}

#[derive(Clone)]
enum RegisterType {
    ReadOnly,
    WriteOnly,
    ReadWrite,
    StateChange(State, Punctuated<syn::Path, syn::Token![,]>, Ident),
    StateChangeRW,
}

impl RegisterType {
    fn to_ident(&self) -> Ident {
        match self {
            RegisterType::ReadOnly => format_ident!("ReadOnly"),
            RegisterType::WriteOnly => format_ident!("WriteOnly"),
            RegisterType::ReadWrite => format_ident!("ReadWrite"),
            RegisterType::StateChange(_, _, _) => format_ident!("StateChange"),
            RegisterType::StateChangeRW => format_ident!("StateChange"),
        }
    }
}

impl Parse for RegisterType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse().expect("regtype parse error");
        match ident.to_string().as_str() {
            "ReadOnly" => Ok(RegisterType::ReadOnly),
            "WriteOnly" => Ok(RegisterType::WriteOnly),
            "ReadWrite" => Ok(RegisterType::ReadWrite),
            "StateChangeRW" => Ok(RegisterType::StateChangeRW),
            "StateChange" => {
                let content;
                let _: syn::token::Paren = syn::parenthesized!(content in input);
                let new_state = content.parse::<State>().expect("registertype 1");

                let _: syn::Token![,] = content.parse().expect("register 2");

                // bracketed with potentially multiple values to set atomically
                // [instr1, instr2, instr3, ..]
                let instructions;
                let _: syn::token::Bracket = syn::bracketed!(instructions in content);
                let instructions: Punctuated<syn::Path, syn::Token![,]> = instructions.
                parse_terminated(syn::Path::parse, syn::Token![,])
            .expect("instruction error");

                if new_state.substates.is_empty() {
                    let state_shortname = new_state.ident.clone();
                    return Ok(RegisterType::StateChange(
                        new_state,
                        instructions,
                        state_shortname,
                    ));
                } else {
                    let _: syn::Token![,] = content
                        .parse()
                        .expect("final comma in register state change");
                    let state_shortname = content.parse::<syn::Ident>().expect("registertype 3");
                    return Ok(RegisterType::StateChange(
                        new_state,
                        instructions,
                        state_shortname,
                    ));
                }
            }
            _ => Err(syn::Error::new(ident.span(), "Unknown register type")),
        }
    }
}

struct Register {
    name: Ident,
    valid_states: Punctuated<State, syn::Token![,]>,
    register_shortname: syn::GenericArgument,
    register_type: RegisterType,
    register_bitwidth: Ident,
}

impl Register {
    fn generate_register_op_bindings(
        &self,
        register_name: &Ident,
    ) -> proc_macro2::TokenStream {
        // impl ReadWriteRegister<#register_bitwidth, #register_shortname, #type_name, #validstate> {
        let register_bitwidth = self.register_bitwidth.clone();
        let register_shortname = self.register_shortname.clone();
        let validstate = self
            .valid_states
            .first()
            .expect("generate reg op bindings")
            .form_concrete_state_type();

        // Determine if this state contains an Any substate.
        let is_anytype = self
            .valid_states
            .first()
            .unwrap()
            .substates
            .iter()
            .any(|substate| substate.to_string() == "Any");

        // An Any substate means an state is valid. To mock up this behavior in the
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
            RegisterType::StateChange(state, instruction, state_shortname) => {
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

struct RegisterAttributes {
    states: Punctuated<State, syn::Token![,]>,
    register_type: RegisterType,
}

impl Parse for RegisterAttributes {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        let _ = bracketed!(content in input);
        let states: Punctuated<State, syn::Token![,]> = content
            .parse_terminated(State::parse, syn::Token![,])
            .expect("1");

        input.parse::<syn::Token![,]>().expect("1");
        let register_type: RegisterType = input.parse().expect("Invalid provided reg type.");

        Ok(RegisterAttributes {
            states,
            register_type,
        })
    }
}

struct MacroInput {
    peripheral_name: String,
    states: Punctuated<State, syn::Token![,]>,
}

impl MacroInput {
    // Gives two returns, the body for the impl Merge<Store> for XXX and
    // the enum store.
    fn generate_state_store(
        &self,
        register_name: &Ident,
        store_name: &Ident,
    ) -> proc_macro2::TokenStream {
        let mut output = proc_macro2::TokenStream::new();

        // Gather substates to be used for creating type.
        let substate_iter = |state: State| {
            state
                .substates
                .iter()
                .map(|substate| {
                    quote! {
                        #substate
                    }
                })
                .collect::<Vec<_>>()
        };

        let substate_tokens = |state: State| {
            let state_ident = state.ident.clone();
            if state.substates.is_empty() {
                quote! {
                        #state_ident
                }
            } else {
                let substate_iter = substate_iter(state.clone());
                quote! {
                    #state_ident<#(#substate_iter),*>
                }
            }
        };

        let store_variants: Vec<Variant> = self
            .states
            .iter()
            .map(|state| {
                let substate_tokens = substate_tokens(state.clone());

                Variant {
                    attrs: Vec::new(),
                    ident: state.shortname.clone(),
                    discriminant: None,
                    fields: Fields::Unnamed(FieldsUnnamed {
                        paren_token: syn::token::Paren(Span::call_site()),
                        unnamed: Punctuated::from_iter(vec![Field {
                            attrs: Vec::new(),
                            vis: Visibility::Inherited,
                            mutability: FieldMutability::None,
                            ident: None,
                            colon_token: None,
                            ty: Type::Verbatim(quote! {
                                #register_name<#substate_tokens>
                            }),
                        }]),
                    }),
                }
            })
            .collect();

        // Expands to:
        // pub enum Nrf5xTempStore {
        //     Off(Nrf5xTempRegister<state_ident>),
        //     Reading(Nrf5xTempRegister<state_ident>),
        // }

        //Nrf5xTemperatureStore::Reading(_reg) => {
        //      Nrf5xTemperatureStore::Reading(Nrf5xTempRegister::<Reading>::new())
        // }
        // Generate StateEnum trait implementation
        let _state_enum_impl = self.states.iter().map(|state| {
            let enum_variant = state.shortname.clone(); 
            let state_tokens = substate_tokens(state.clone());
            quote! {
                #store_name::#enum_variant(_) => #store_name::#enum_variant(#register_name::<#state_tokens>::new())
            }
        });

        let sync_state_body = self.states.iter().map(|state| {
            let enum_variant = state.shortname.clone();
            let _state_tokens = substate_tokens(state.clone());
            quote! {
                #store_name::#enum_variant(reg) => reg.sync_state()
            }
        });

        let _debug_body = self.states.iter().map(|state| {
            let enum_variant = state.shortname.clone();
            let _state_tokens = substate_tokens(state.clone());
            quote! {
                #store_name::#enum_variant(reg) => write!(f, "{}", stringify!(#enum_variant))
            }
        });

        output.extend(quote! {
            pub enum #store_name{
                #(#store_variants),*
            }

            impl Store for #store_name {}
            impl StateEnum for #store_name {

                fn sync_state(self) -> Self {
                    match self {
                        #(#sync_state_body),*
                    }
                }
            }

            // impl core::fmt::Debug for #store_name {
            //     fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            //         match self {
            //             #(#debug_body),*
            //         }
            //     }
            // }

        });

        output
    }

    fn generate_states(
        &self,
        register_name: &Ident,
        store_name: &Ident,
    ) -> (proc_macro2::TokenStream, HashMap<String, State>) {
        let mut output = proc_macro2::TokenStream::new();

        // The other state hashes include the substates. This is strictly
        // looking at the State name
        let mut strict_state_hash = HashSet::new();

        let mut created_states: HashSet<syn::Ident> = HashSet::new();

        let mut unique_substates = HashSet::new();
        unique_substates.insert(format_ident!("Any"));

        let mut state_hash = HashSet::new();

        let all_states_vec = self.states.iter().collect::<Vec<_>>();

        let mut state_map = HashMap::new();
        for state in &self.states {
            if !strict_state_hash.contains(state.ident.to_string().as_str()) {
                let state_ident = state.ident.clone();
                if state.substates.is_empty() {
                    output.extend(quote! {
                        impl <A1> Merge<#register_name<A1>> for #register_name<#state_ident>
                        where
                            #state_ident: State,
                            A1: State,
                        {
                            type Output = #register_name<#state_ident>;
                            fn merge(self, _other: #register_name<A1>) -> Self::Output {
                                self
                            }
                        }
                    });
                } else if state.substates.len() == 1 {
                    output.extend(
                        quote!{
                            impl <A1, B1> Merge<#register_name<#state_ident<A1>>> for #register_name<#state_ident<B1>> 
                            where 
                                #state_ident<A1>: State,
                                #state_ident<B1>: State,
                                A1: SubState + MergeSubState<A1, B1>,
                                B1: SubState + ConcreteSubState,
                                #state_ident<
                                    <A1 as MergeSubState<A1, B1>>::Output>: State,
                                {
                                    type Output = #register_name<#state_ident<
                                        <A1 as MergeSubState<A1, B1>>::Output
                                    >>;
                                    
                                    fn merge(self, _other: #register_name<#state_ident<A1>>) -> Self::Output {
                                        unsafe {
                                            transmute::<#register_name<#state_ident<B1>>, Self::Output>(self) 
                                        }
                                    }

                                }
                        }
                    )
                } else if state.substates.len() == 2 {
                    output.extend(
                        quote!{
                            impl <A1, A2, B1, B2> Merge<#register_name<#state_ident<A1, A2>>> for #register_name<#state_ident<B1, B2>>
                            where 
                                A1: SubState + MergeSubState<A1, B1>,
                                A2: SubState + MergeSubState<A2, B2>,
                                B1: SubState + ConcreteSubState,
                                B2: SubState + ConcreteSubState,
                                #state_ident<A1, A2>: State,
                                #state_ident<B1, B2>: State,
                                #state_ident<
                                    <A1 as MergeSubState<A1, B1>>::Output,
                                    <A2 as MergeSubState<A2, B2>>::Output
                                >: State,
                                {
                                    type Output = #register_name<#state_ident<
                                        <A1 as MergeSubState<A1, B1>>::Output,
                                        <A2 as MergeSubState<A2, B2>>::Output
                                    >>;
                                    
                                    fn merge(self, _other: #register_name<#state_ident<A1, A2>>) -> Self::Output {
                                        unsafe {
                                            transmute::<#register_name<#state_ident<B1, B2>>, Self::Output>(self) 
                                        }
                                    }

                                }
                        }
                    );
                } else {
                    unimplemented!("Only 2 substates are supported.");
                }

                strict_state_hash.insert(state.ident.to_string());
            }

            // State hash map used for name mapping later.
            state_map.insert(state.form_concrete_state_type().to_string(), state.clone());

            let mut state = state.clone();
            let original_substates = state.substates.clone();
            // We need to generate each substate as:
            // 1. The specified state.
            // 2. As potentially an any state.
            // 3. As potentially all being any states.
            // TODO: We need to add logic for if there are 3 substates (e.g. <Any, Any, Tx>)
            for iter in 0..(&state.substates.len() + 2) {
                let mut any_positions: Option<Vec<(usize, Ident)>> = None;

                state.substates = original_substates.clone();

                // Case (2)
                if iter < state.substates.len() {
                    any_positions = Some(vec![(iter, state.substates[iter].clone())]);
                    state.substates[iter] = format_ident!("Any");
                }

                // Case (3)
                if iter == state.substates.len() {
                    // Update substates to all be "Any" and record positions with prior ident value
                    let mut vec: Vec<(usize, Ident)> = Vec::new();
                    for (pos, substate) in state.substates.iter().enumerate() {
                        vec.push((pos, substate.clone()));
                    }

                    any_positions = Some(vec);
                    state
                        .substates
                        .iter_mut()
                        .for_each(|substate| *substate = format_ident!("Any"));
                }

                let state_ident = state.ident.clone();

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

                if !created_states.contains(&state.ident) {
                    output.extend(quote! {
                        pub struct #struct_name {
                            #(#fields),*
                        }
                    });

                    created_states.insert(state.ident.clone());
                }

                output.extend(state.generate_state(
                    register_name,
                    store_name,
                    &struct_name,
                    all_states_vec.clone(),
                    any_positions,
                ));
            }
        }

        // create substates
        for substate in unique_substates {
            let substate_ident = format_ident!("{}", substate);
            let any_trait = if "Any" == substate_ident.to_string() {
                quote! {
                    impl AnySubState for #substate_ident {}
                }
            } else {
                quote! {
                    impl ConcreteSubState for #substate_ident {}
                }
            };

            output.extend(quote! {
                pub struct #substate_ident {}

                #any_trait

                impl SubState for #substate_ident {}
            });
        }

        (output, state_map)
    }

}

fn add_imports() -> proc_macro2::TokenStream {
    quote!(
        use abacus_mmio::{
            State, SubState, StateEnum, Reg, Store, Merge,
            AnyReg, SyncState, AnySubState, ConcreteSubState, MergeSubState 
        };
        use core::marker::PhantomData;
        use core::mem::transmute;
        use core::ops::Deref;
        use abacus_mmio::tock_registers::{
           fields::FieldValue, UIntLike, RegisterLongName, fields::Field, LocalRegisterCopy,
           interfaces::ReadWriteable,
        };
        use core::convert::TryFrom;
    )
}

impl Parse for MacroInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let _: custom_keywords::peripheral_name = input.parse().expect("macinput err");
        let _: syn::Token![=] = input.parse().expect("macinput err");
        let peripheral_name: syn::LitStr = input.parse().expect("macinput err");
        let _: syn::Token![,] = input.parse().expect("macinput err");

        let _: custom_keywords::register_base_addr = input.parse().expect("macinput err");
        let _: syn::Token![=] = input.parse().expect("macinput err");
        let _base_addr: syn::LitInt = input.parse().expect("error with base addr");
        let _: syn::Token![,] = input.parse().expect("macinput err");

        let _: custom_keywords::states = input.parse().expect("macinput err");
        let _: syn::Token![=] = input.parse().expect("macinput err");
        let states_content;
        let _: syn::token::Bracket = bracketed!(states_content in input);
        let states: Punctuated<State, syn::Token![,]> = states_content
            .parse_terminated(State::parse, syn::Token![,])
            .expect("macinput err");

        Ok(MacroInput {
            peripheral_name: peripheral_name.value(),
            states,
        })
    }
}

#[proc_macro_attribute]
pub fn process_register_block(attr: TokenStream, item: TokenStream) -> TokenStream {
    let parsed_input = parse_macro_input!(attr as MacroInput);
    // form reg and store type names from given peripheral name
    let register = format_ident!("{}Registers", parsed_input.peripheral_name);
    let internal_register = format_ident!("block");
    let store = format_ident!("{}Store", parsed_input.peripheral_name);

    let mut result = add_imports();


// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2022.


    


    // IN REGARDS TO THE NEW METHOD BELOW:
    // The existence of this method destroys all guarantees. We need this
    // to store the anytype, but need to do this in a controlled way so that
    // we don't allow anyone to "escape" the power manager.
    let block = quote! {
        pub struct #register<S: State> {
            reg:  AbacusStaticRef<#internal_register<S>>,
        }

         impl <S: State> #register<S> {
             pub const unsafe fn new(base_addr: usize) -> #register<S> {
                 let reg = unsafe { AbacusStaticRef::new(base_addr as *const #internal_register<S>) };
                 #register { reg } 
             }
         }

        impl <S: State> Deref for #register<S> {
            type Target = #internal_register<S>;
            fn deref(&self) -> &#internal_register<S> {
                self.reg.deref()
            }
        }
    };

    result.extend(block);

    // Generate store enum
    let state_enum = parsed_input.generate_state_store(&register, &store);
    result.extend(state_enum);

    // Generate states
    let (states_generated, _state_map) = parsed_input.generate_states(&register, &store);
    result.extend(states_generated);

    // result.extend(parsed_input.generate_disjunctive_states());

    let ast: DeriveInput = syn::parse(item).expect("ast unwrap");

    let data = match &ast.data {
        syn::Data::Struct(data) => data,
        _ => panic!("Unsupported data type"),
    };

    let mut reg_vec: Vec<Register> = vec![];

    // Iterator that is applied to each field in the register struct. Used to convert a
    // tock register struct into an "abacus" struct.
    let field_details = data.fields.iter().map(|field| {
        let field_type = field.ty.clone();
        let field_name = field.ident.clone().expect("field details");

        // If there is a regattribute, we will need to generate bindings.
        let requires_gen = field.attrs.iter().any(|attr| {
            // for each attribute in field attrs, leave all macros but
            // RegAttributes.
            if attr.path().is_ident("RegAttributes") {
                return true;
            }
            false
        });

        let field_attr = field.attrs.iter().map(|attr| {
            // for each attribute in field attrs, leave all macros but
            // RegAttributes.
            if attr.path().is_ident("RegAttributes") {
                return quote!{};
            } else {
                return quote! {#attr};
            }
        });


        // DO ATTRIBUTE EXPANSION HERE
        if requires_gen {

            // Parse each register attribute.
            let reg_attr_vec = field.attrs.iter().map(|attr| {
                // for each attribute in field attrs, leave doc macro comments
                // and remove RegAttributes.
                if attr.path().is_ident("RegAttributes") {
                    if let Ok(val) = attr.parse_args::<RegisterAttributes>() {
                        return Some(val);
                    } else {
                        return None;
                    }
                }
                None
            }).collect::<Vec<_>>();

            // To properly form the register bindings, we must also take information
            // from the provided type.
            if let Type::Path(type_path) = field_type.clone() {
                if let Some(segment) = type_path.path.segments.last() {

                    // Check for generics in original struct's type.
                    if let PathArguments::AngleBracketed(args) = &segment.arguments {
                        let generic_args = &args.args;
                        if generic_args.len() != 2 {
                            panic!("Expected 2 generic arguments");
                        }
                        let register_shortname = generic_args[1].clone();

                        let register_bitwidth =
                        if let syn::GenericArgument::Type(Type::Path(type_path)) = &generic_args[0]
                        {
                            let generic_ident = &type_path.path;
                            generic_ident.segments.first().expect("inner req gen").ident.clone()
                        } else {
                            panic!("unreachable");
                        };

                    let mut output = proc_macro2::TokenStream::new();

                    let mut first_item = true;
                    // For each register field, we iterate through all the register 
                    // attributes and create a Register object for each attribute.
                    for reg_attr in reg_attr_vec {
                        if let Some(reg_attr) = reg_attr {
                            reg_vec.push(Register {
                                name: field_name.clone(),
                                valid_states: reg_attr.states,
                                register_shortname: register_shortname.clone(),
                                register_type: reg_attr.register_type.clone(),
                                register_bitwidth: register_bitwidth.clone(),
                            });
                            
                            let reg_type = format_ident!("{}Register", &reg_attr.register_type.to_ident());

                            
                            if first_item {
                                let field_attr_clone = field_attr.clone();
                                output.extend(
                                    quote!{
                                        #(#field_attr_clone)*
                                        pub #field_name: #reg_type<#register_bitwidth, #register_shortname, S>
                                    }
                                );

                                first_item = false;
                            }
                        }
                    }                    
                    output
                } else {
                    panic!("unreachable a")
                }
            } else {
                panic!("unreachable b");
            }
        } else {
            panic!("unreachable c");
        }
    } else {
        quote! {
            #(#field_attr)*
            pub #field_name: #field_type

        }
    }

});

    let struct_output = quote! {
        #[repr(C)]
        pub struct #internal_register<S: State> {
            #(#field_details),*
        }
    };

    let mut generated_bindings_set = HashSet::new();
    for reg in reg_vec {
        //  result.extend(reg.generate_state_transition(&peripheral, &register));
        let new_binding = reg.generate_register_op_bindings(&register);
        if generated_bindings_set.insert(new_binding.clone().to_string()) {
            result.extend(new_binding.clone());
        }
    }

    // FIX ME
    result.extend(quote! {
        pub enum RegisterResult<A: Reg, B: Reg> {
            Ok(A),
            Err(B),
        }

        impl <A: Reg, B: Reg> From<Result<A, B>> for RegisterResult<A, B> {
            fn from(result: Result<A, B>) -> Self {
                match result {
                    Ok(val) => RegisterResult::Ok(val),
                    Err(err) => RegisterResult::Err(err),
                }
            }
        }

        impl <A: Reg, B: Reg> RegisterResult<A, B> {
            fn into_closure_return<C, D>(self) -> Result<C, D> 
            where
                C: StateEnum + From<A>,
                D: StateEnum + From<B>,
            {
                match self {
                    RegisterResult::Ok(val) => Ok(val.into()),
                    RegisterResult::Err(reg) => Err(reg.into())
                }
            }
        }

        pub struct StateChangeRegister<T: UIntLike, R: RegisterLongName, S: State> {
            reg: ReadWrite<T, R>,
            associated_state: PhantomData<S>,
        }

        struct ReadWriteRegister<T: UIntLike, R: RegisterLongName, S: State> {
            reg: ReadWrite<T, R>,
            associated_state: PhantomData<S>,
        }

        struct ReadOnlyRegister<T: UIntLike, R: RegisterLongName, S: State> {
            reg: ReadOnly<T, R>,
            associated_state: PhantomData<S>,
        }

        struct WriteOnlyRegister<T: UIntLike, R: RegisterLongName, S: State> {
            reg: WriteOnly<T, R>,
            associated_state: PhantomData<S>,
        }

        // Implement SubState Merge logic
        impl<T: SubState> MergeSubState<Any, T> for Any {
            type Output = T;
        }

        
        // impl<A, B> MergeSubState<A, B> for A {
        //     type Output = A;
        // }


    });

    result.extend(struct_output);
    result.into()
}

// This is a helper function to determine if two states are mergable.
fn is_mergeable(state1: &State, state2: &State) -> bool {
    // Given 2 reg types, determine if they are mergeable

    // If the states are NOT the same, they are not mergeable.
    if state1.ident != state2.ident {
        return false;
    }

    // For every substate, the substates may only be different if
    // one of the substates is ANY.
    // for (substate1, substate2) in state1.substates.iter().zip(state2.substates.iter()) {
    //     if substate1 != substate2 {
    //         if substate1.to_string() != "Any" && substate2.to_string() != "Any" {
    //             return false;
    //         }
    //     }
    // }

    return true;
}

// This is a helper function to determine if a state is valid to coerce into another
// for usage with try_into (e.g. Active<Any, Tx> => Active<Rx, Tx>).
fn is_valid_into(state1: &State, state2: &State) -> bool {
    // Given 2 reg types, determine if they are mergeable

    // If the states are NOT the same, they are not mergeable.
    if state1.ident != state2.ident {
        return false;
    }

    // For every substate, the substates may only be different if
    // one of the substates is ANY.
    for (substate1, substate2) in state1.substates.iter().zip(state2.substates.iter()) {
        if substate1 != substate2 {
            if substate1.to_string() != "Any" && substate2.to_string() != "Any" {
                return false;
            }
        }
    }

    return true;
}
#[proc_macro_attribute]
pub fn entry_point(attr: TokenStream, item: TokenStream) -> TokenStream {
    // This is only valid to be placed upon functions.
    let ast: ItemFn = syn::parse(item).expect("entry point unwrap");

    let function_sig = &ast.sig;
    let function_block = &ast.block.stmts;
    let function_vis = &ast.vis;

    // Get field of register struct provided as #[entry_point(NAME)]
    let entry_point_name = parse_macro_input!(attr as syn::LitStr).value();
    let reg_field_ident = format_ident!("{}", entry_point_name);

    // We expect this to be a member function of the struct that contains
    // the power manager. (TODO: rethink this assumption)
    let check_interrupts_shim = quote! {
        self.#reg_field_ident.take().map(|state| self.#reg_field_ident.replace(state.sync_state()));
    };

    // Prepend check_interrupts_shim to body of fn.
    quote! {
        #function_vis #function_sig {
            #check_interrupts_shim
            #(#function_block)*
        }
    }
    .into()
}

fn merge_constraint_vec(
    vec1: Vec<proc_macro2::TokenStream>,
    vec2: Vec<proc_macro2::TokenStream>,
) -> Vec<proc_macro2::TokenStream> {
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

