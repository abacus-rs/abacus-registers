use quote::format_ident;
use syn::{
    bracketed,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Ident,
};

/// Helper struct for parsing macro input (Abacus DSL) and
/// used later for code gen.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct State {
    pub state_name: syn::Ident,
    pub substates: Punctuated<syn::Ident, syn::Token![,]>,
}


/// Struct to store parsed state definition.
#[derive(Clone, Debug)]
pub struct StateDefinition {
    pub state: State,
    pub transient: bool,
    pub state_shortname: syn::Ident,
}

impl Parse for State {
    /// Function to Parse a provided State and Transition.
    ///
    /// Example Inputs:
    /// State
    /// State(SubState)
    /// State(SubState1, SubState2)
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let state_name: Ident = input.parse()
            .map_err(|_| syn::Error::new(input.span(), "Abacus Macro Error - State Parsing declaration missing. Valid states are of the form `State`, `State(SubState)`, `State(SubState1, SubState2)`"))?;

        // Check for substates - of the form State(SubState, ..., SubStateN)
        let substates = if input.peek(syn::token::Paren) {
            let substate_def;
            let _: syn::token::Paren = syn::parenthesized!(substate_def in input);
            let substates: Punctuated<Ident, syn::Token![,]> = substate_def
                .parse_terminated(syn::Ident::parse, syn::Token![,])
                .map_err(|_| syn::Error::new(substate_def.span(), "Abacus Macro Error - SubState Parsing declaration invalid. Valid state/substates are of the form `State`, `State(SubState)`, `State(SubState1, SubState2)`"))?;
            Some(substates)
        } else { None }
        .unwrap_or_else(|| Punctuated::new());

        Ok(State {
            state_name,
            substates,
        })
    }
}

impl Parse for StateDefinition {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // State definition is of the form:
        // (STATE_NAME, optional_shortname, optional_*T*)
        //
        // - (State) - simple state, no shortname, not transient
        // - (State(SubState1, ..., SubStateN), shortname) - state with substates, shortname required
        // - (State, *T*) - transient state
        // - (State(SubState1, ..., SubStateN), shortname, *T*) - transient state with substates

        let state_origin_definition;
        let _: syn::token::Paren = syn::parenthesized!(state_origin_definition in input);
        let state = state_origin_definition.parse::<State>().map_err(|_| {
            syn::Error::new(
                state_origin_definition.span(),
                "Abacus Macro - Error Parsing State.",
            )
        })?;

        let state_shortname = if !state.substates.is_empty() {
            // States with SubState must provide a "shortname" of the form
            // (State(Substates...), shortname).
            let _: syn::token::Comma = state_origin_definition.parse()
                .map_err(|_| syn::Error::new(state_origin_definition.span(), "Abacus Macro Error - State Definition Parsing, no comma between state(substate1, ..., substateN) and shortname. Should be of the form `(State(SubState1, ..., SubState2), shortname)`"))?;

            state_origin_definition.parse().map_err(|_| {
                syn::Error::new(
                    state_origin_definition.span(),
                    "Abacus Macro Error - State Parsing, shortname not provided.",
                )
            })?
        } else {
            state.state_name.clone()
        };

        // Optional *T* transient marker in the remaining parenthesized content.
        let mut transient = false;
        if state_origin_definition.parse::<syn::Token![,]>().is_ok() {
            let _: syn::Token![*] = state_origin_definition.parse().map_err(|e| {
                syn::Error::new(e.span(), "Abacus Macro Error - Expected * at start of transient marker *T*. Should be of the form `(State, *T*)`")
            })?;
            let t_ident: Ident = state_origin_definition.parse().map_err(|e| {
                syn::Error::new(e.span(), "Abacus Macro Error - Expected 'T' in transient marker *T*. Should be of the form `(State, *T*)`")
            })?;
            if t_ident.to_string() != "T" {
                return Err(syn::Error::new(
                    t_ident.span(),
                    format!("Abacus Macro Error - Transient marker *T* must be followed by 'T'. Instead found: {}", t_ident),
                ));
            }
            let _: syn::Token![*] = state_origin_definition.parse().map_err(|_| syn::Error::new(state_origin_definition.span(), "Abacus Macro Error - Error Parsing transient marker *T*. Should be of the form `(State, *T*)`"))?;
            transient = true;
        }

        // Check if anything else is left in the state definition
        if !state_origin_definition.is_empty() {
            return Err(syn::Error::new(
                state_origin_definition.span(),
                format!("Abacus Macro Error - Transient marker *T* must be followed by nothing else. Instead found tokens: {}", state_origin_definition),
            ));
        }

        // Reject trailing tokens that aren't a comma (next item in states list).
        if !input.is_empty() && !input.peek(syn::Token![,]) {
            return Err(syn::Error::new(
                input.span(),
                "Abacus Macro Error - Unexpected tokens after state definition. Each `(State, ...)` entry must be separated by commas.",
            ));
        }

        Ok(StateDefinition {
            state,
            transient,
            state_shortname,
        })
    }
}

#[derive(Clone)]
pub enum RegisterType {
    ReadOnly,
    WriteOnly,
    ReadWrite,
    /// StateChange(new_state, [instructions], shortname?)
    /// shortname is used for trait Step{Shortname} and method into_{shortname}
    StateChange(State, Punctuated<syn::Path, syn::Token![,]>, Option<syn::Ident>),
    StateChangeRW,
}

impl RegisterType {
    pub fn to_ident(&self) -> Ident {
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
        let ident: Ident = input.parse()
            .map_err(|_| syn::Error::new(
                input.span(), 
                "Error parsing register type - expected register type (e.g. ReadOnly, WriteOnly) not provided.")
            )?;

        match ident.to_string().as_str() {
            "ReadOnly" => Ok(RegisterType::ReadOnly),
            "WriteOnly" => Ok(RegisterType::WriteOnly),
            "ReadWrite" => Ok(RegisterType::ReadWrite),
            "StateChange" => {
                let content;
                let _: syn::token::Paren = syn::parenthesized!(content in input);
                let new_state = content.parse::<State>().map_err(|_| {
                    syn::Error::new(
                        content.span(),
                        "Error parsing state in StateChange Register Attribute.",
                    )
                })?;

                let _: syn::Token![,] = content.parse()
                    .map_err(|_| syn::Error::new(content.span(), "Error in Register Attribute, State Change; missing `,` between the new state and bit field values that result in the given transition."))?;

                // Bracketed list of bitfield values that will result in this
                // state transition.
                let bitfields;
                let _: syn::token::Bracket = syn::bracketed!(bitfields in content);
                let bitfield_instr: Punctuated<syn::Path, syn::Token![,]> = bitfields
                    .parse_terminated(syn::Path::parse, syn::Token![,])
                    .map_err(|_| {
                        syn::Error::new(
                            bitfields.span(),
                            "Error parsing the state change bitfield list.",
                        )
                    })?;

                // Optional shortname for method (e.g. transmit, receive) - used for into_transmit, into_receive
                let shortname = if content.parse::<syn::Token![,]>().is_ok() {
                    Some(content.parse::<syn::Ident>().map_err(|_| {
                        syn::Error::new(
                            content.span(),
                            "Error in StateChange: expected identifier (shortname) after comma.",
                        )
                    })?)
                } else {
                    None
                };

                return Ok(RegisterType::StateChange(new_state, bitfield_instr, shortname));
            }
            _ => Err(syn::Error::new(
                ident.span(),
                "Unknown register type provided in RegAttribute",
            )),
        }
    }
}

pub struct RegisterAttributes {
    pub valid_states: Punctuated<State, syn::Token![,]>,
    pub register_type: RegisterType,
}

impl Parse for RegisterAttributes {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        let _ = bracketed!(content in input);
        let valid_states: Punctuated<State, syn::Token![,]> = content
            .parse_terminated(State::parse, syn::Token![,])
            .map_err(|state_err| {
                syn::Error::new(
                    state_err.span(),
                    format!("Register attribute error: {}", state_err),
                )
            })?;

        input.parse::<syn::Token![,]>().map_err(|_| {
            syn::Error::new(
                input.span(),
                "Abacus Macro - Expected comma after defining valid states in register attributes.",
            )
        })?;
        let register_type: RegisterType = input.parse().map_err(|reg_type_err| {
            syn::Error::new(
                input.span(),
                format!("Register attribute error: {}", reg_type_err),
            )
        })?;

        Ok(RegisterAttributes {
            valid_states,
            register_type,
        })
    }
}

// Unit tests for parsing logic.
#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;
    use syn::parse_quote;

    #[test]
    fn test_state_parse_simple_success() {
        let input = quote! { Off };
        let state: State = syn::parse2(input).unwrap();

        assert_eq!(state.state_name.to_string(), "Off");
        assert!(state.substates.is_empty());
    }

    #[test]
    fn test_state_parse_with_single_substate() {
        let input = quote! { Active(Tx) };
        let state: State = syn::parse2(input).unwrap();

        assert_eq!(state.state_name.to_string(), "Active");
        assert_eq!(state.substates.len(), 1);
        assert_eq!(state.substates.first().unwrap().to_string(), "Tx");
    }

    #[test]
    fn test_state_parse_with_multiple_substates() {
        let input = quote! { Active(Tx, Rx) };
        let state: State = syn::parse2(input).unwrap();

        assert_eq!(state.state_name.to_string(), "Active");
        assert_eq!(state.substates.len(), 2);
        let substates: Vec<String> = state.substates.iter().map(|s| s.to_string()).collect();
        assert_eq!(substates, vec!["Tx", "Rx"]);
    }

    #[test]
    fn test_state_parse_missing_identifier_error() {
        let input = quote! {};
        let result: syn::Result<State> = syn::parse2(input);

        assert!(result.is_err());
    }

    #[test]
    fn test_register_type_parse_read_only() {
        let input = quote! { ReadOnly };
        let reg_type: RegisterType = syn::parse2(input).unwrap();

        match reg_type {
            RegisterType::ReadOnly => {}
            _ => panic!("Expected ReadOnly register type"),
        }
    }

    #[test]
    fn test_register_type_parse_write_only() {
        let input = quote! { WriteOnly };
        let reg_type: RegisterType = syn::parse2(input).unwrap();

        match reg_type {
            RegisterType::WriteOnly => {}
            _ => panic!("Expected WriteOnly register type"),
        }
    }

    #[test]
    fn test_register_type_parse_read_write() {
        let input = quote! { ReadWrite };
        let reg_type: RegisterType = syn::parse2(input).unwrap();

        match reg_type {
            RegisterType::ReadWrite => {}
            _ => panic!("Expected ReadWrite register type"),
        }
    }

    #[test]
    fn test_register_type_parse_state_change() {
        let input = quote! { StateChange(Active, [ENABLE::SET, MODE::CONTINUOUS]) };
        let reg_type: RegisterType = syn::parse2(input).unwrap();

        match reg_type {
            RegisterType::StateChange(state, instructions, shortname) => {
                assert_eq!(state.state_name.to_string(), "Active");
                assert!(shortname.is_none());
                assert_eq!(instructions.len(), 2);

                let instruction_str: String = instructions
                    .iter()
                    .map(|path| quote::quote!(#path).to_string())
                    .collect::<Vec<_>>()
                    .join(" ");
                assert!(instruction_str.contains("ENABLE"));
                assert!(instruction_str.contains("SET"));
                assert!(instruction_str.contains("MODE"));
                assert!(instruction_str.contains("CONTINUOUS"));
            }
            _ => panic!("Expected StateChange register type"),
        }
    }

    #[test]
    fn test_register_type_parse_state_change_with_shortname() {
        let input = quote! { StateChange(On(Transient), [Task::ENABLE::SET], transmit) };
        let reg_type: RegisterType = syn::parse2(input).unwrap();

        match reg_type {
            RegisterType::StateChange(state, instructions, shortname) => {
                assert_eq!(state.state_name.to_string(), "On");
                assert_eq!(instructions.len(), 1);
                assert_eq!(shortname.as_ref().unwrap().to_string(), "transmit");
            }
            _ => panic!("Expected StateChange register type"),
        }
    }

    #[test]
    fn test_register_type_parse_unknown_error() {
        let input = quote! { ReallyFunRegType };
        let result: syn::Result<RegisterType> = syn::parse2(input);

        assert!(result.is_err());
    }

    #[test]
    fn test_register_attributes_parse_success() {
        let input = quote! { [Off, Active], ReadOnly };
        let reg_attrs: RegisterAttributes = syn::parse2(input).unwrap();

        assert_eq!(reg_attrs.valid_states.len(), 2);
        let state_names: Vec<String> = reg_attrs
            .valid_states
            .iter()
            .map(|s| s.state_name.to_string())
            .collect();
        assert_eq!(state_names, vec!["Off", "Active"]);

        match reg_attrs.register_type {
            RegisterType::ReadOnly => {}
            _ => panic!("Expected ReadOnly register type"),
        }
    }

    #[test]
    fn test_register_attributes_parse_with_substates() {
        let input = quote! { [Off, Active(Tx, Rx)], WriteOnly };
        let reg_attrs: RegisterAttributes = syn::parse2(input).unwrap();

        assert_eq!(reg_attrs.valid_states.len(), 2);

        // Check first state (simple)
        let first_state = &reg_attrs.valid_states[0];
        assert_eq!(first_state.state_name.to_string(), "Off");
        assert!(first_state.substates.is_empty());

        // Check second state (with substates)
        let second_state = &reg_attrs.valid_states[1];
        assert_eq!(second_state.state_name.to_string(), "Active");
        assert_eq!(second_state.substates.len(), 2);
    }

    #[test]
    fn test_register_attributes_missing_comma_error() {
        let input = quote! { [Off, Active] ReadOnly };
        let result: syn::Result<RegisterAttributes> = syn::parse2(input);

        assert!(result.is_err());
    }

    #[test]
    fn test_register_attributes_invalid_register_type_error() {
        let input = quote! { [Off, Active], InvalidRegisterType };
        let result: syn::Result<RegisterAttributes> = syn::parse2(input);

        assert!(result.is_err());
    }

    #[test]
    fn test_state_origin_definition_parse_simple() {
        let input = quote! { (Off) };
        let state_def: StateDefinition = syn::parse2(input).unwrap();

        assert_eq!(state_def.state.state_name.to_string(), "Off");
        assert_eq!(state_def.state_shortname.to_string(), "Off");
        assert!(!state_def.transient);
    }

    #[test]
    fn test_state_origin_definition_parse_with_substates() {
        let input = quote! { (Active(Tx, Rx), ActiveTxRx) };
        let state_def: StateDefinition = syn::parse2(input).unwrap();

        assert_eq!(state_def.state.state_name.to_string(), "Active");
        assert_eq!(state_def.state_shortname.to_string(), "ActiveTxRx");
        assert!(!state_def.transient);
        assert_eq!(state_def.state.substates.len(), 2);
    }

    #[test]
    fn test_state_origin_definition_missing_paren_error() {
        let input = quote! { Off };
        let result: syn::Result<StateDefinition> = syn::parse2(input);

        assert!(result.is_err());
    }

    #[test]
    fn test_state_origin_definition_transient_true() {
        let input = quote! { (Loading, *T*) };
        let state_def: StateDefinition = syn::parse2(input).unwrap();

        assert_eq!(state_def.state.state_name.to_string(), "Loading");
        assert_eq!(state_def.state_shortname.to_string(), "Loading");
        assert!(state_def.transient);
    }

    #[test]
    fn test_state_origin_definition_transient_with_substates() {
        let input = quote! { (Active(Tx, Rx), ActiveTxRx, *T*) };
        let state_def: StateDefinition = syn::parse2(input).unwrap();

        assert_eq!(state_def.state.state_name.to_string(), "Active");
        assert_eq!(state_def.state_shortname.to_string(), "ActiveTxRx");
        assert!(state_def.transient);
        assert_eq!(state_def.state.substates.len(), 2);
    }

    #[test]
    fn test_state_origin_definition_transient_in_parentheses() {
        let input = quote! { (Loading, *T*) };
        let state_def: StateDefinition = syn::parse2(input).unwrap();

        assert_eq!(state_def.state.state_name.to_string(), "Loading");
        assert_eq!(state_def.state_shortname.to_string(), "Loading");
        assert!(state_def.transient);
    }

    #[test]
    fn test_state_origin_definition_invalid_transient_marker_in_parentheses() {
        let input = quote! { (Loading, *X*) };
        let result: syn::Result<StateDefinition> = syn::parse2(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_state_origin_definition_substates_invalid_transient_marker_in_parentheses() {
        // *X* is not recognized as transient (only *T* sets transient)
        let input = quote! { (Loading(SubState), Shortname, *X*) };
        let result: syn::Result<StateDefinition> = syn::parse2(input);

        assert!(result.is_err());
    }

    #[test]
    fn test_register_type_state_change_missing_comma_error() {
        let input = quote! { StateChange(Active [ENABLE::SET]) };
        let result: syn::Result<RegisterType> = syn::parse2(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_register_type_to_ident() {
        assert_eq!(RegisterType::ReadOnly.to_ident().to_string(), "ReadOnly");
        assert_eq!(RegisterType::WriteOnly.to_ident().to_string(), "WriteOnly");
        assert_eq!(RegisterType::ReadWrite.to_ident().to_string(), "ReadWrite");
        assert_eq!(
            RegisterType::StateChangeRW.to_ident().to_string(),
            "StateChange"
        );

        let state_change = RegisterType::StateChange(
            State {
                state_name: parse_quote!(Active),
                substates: Punctuated::new(),
            },
            Punctuated::new(),
            None,
        );
        assert_eq!(state_change.to_ident().to_string(), "StateChange");
    }

    #[test]
    fn test_state_definition_no_trailing() {
        let input = quote! { (Off) Other};
        
        let result: syn::Result<StateDefinition> = syn::parse2(input);
        let err = result.unwrap_err();
        assert!(err.to_string().contains("Unexpected tokens after state definition"));
    }
}
