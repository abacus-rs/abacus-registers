use quote::format_ident;
use syn::{
    bracketed,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Ident,
};

/// Helper trait for parsing macro input (Abacus DSL) and
/// used later for code gen.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct State {
    pub state_name: syn::Ident,
    pub substates: Punctuated<syn::Ident, syn::Token![,]>,
}

pub struct StateDefinition {
    pub state: State,
    pub transient: bool,
    pub state_shortname: syn::Ident,
}

impl Parse for State {
    /// Function to Parse a provided State and Transition.
    ///
    /// Example Inputs:
    /// State => [State1, ..., StateN]
    /// State(SubState) => [State1, ..., StateN]
    ///
    /// State(SubState1, SubState2) => [State1, ..., StateN]
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
        let state_definition;
        let _: syn::token::Paren = syn::parenthesized!(state_definition in input);
        let state = state_definition.parse::<State>().map_err(|_| {
            syn::Error::new(
                state_definition.span(),
                "Abacus Macro - Error Parsing State.",
            )
        })?;

        // First, check for *T* marker anywhere in the remaining parenthesized content
        let mut transient = false;
        if state_definition.peek(syn::Token![,]) {
            // Look ahead to see if there's a *T* marker after comma(s)
            let fork = state_definition.fork();
            let temp_fork = fork;

            // Skip potential comma and shortname to look for *T*
            while temp_fork.peek(syn::Token![,]) || temp_fork.peek(syn::Ident) {
                if temp_fork.peek(syn::Token![,]) {
                    let _: syn::Token![,] = temp_fork.parse().unwrap();
                } else if temp_fork.peek(syn::Ident) {
                    let _: syn::Ident = temp_fork.parse().unwrap();
                }

                // Check for *T* pattern
                if temp_fork.peek(syn::Token![*])
                    && temp_fork.peek2(syn::Ident)
                    && temp_fork.peek3(syn::Token![*])
                {
                    let _: syn::Token![*] = temp_fork.parse().unwrap();
                    let t_ident: syn::Ident = temp_fork.parse().unwrap();
                    let _: syn::Token![*] = temp_fork.parse().unwrap();

                    if t_ident.to_string() == "T" {
                        transient = true;
                        break;
                    }
                }
            }
        }

        let state_shortname = if !state.substates.is_empty() {
            // States with SubState must provide a "shortname" of the form
            // (State(Substates...), shortname).
            let _: syn::token::Comma = state_definition.parse()
                .map_err(|_| syn::Error::new(state_definition.span(), "Abacus Macro Error - State Definition Parsing, no comma between state(substate1, ..., substateN) and shortname. Should be of the form `(State(SubState1, ..., SubState2), shortname)`"))?;

            let short = state_definition.parse().map_err(|_| {
                syn::Error::new(
                    state_definition.span(),
                    "Abacus Macro Error - State Parsing, shortname not provided.",
                )
            })?;
            // Optional ", *T*" after shortname
            if state_definition.peek(syn::Token![,]) {
                let _: syn::Token![,] = state_definition.parse()?;
                if state_definition.peek(syn::Token![*])
                    && state_definition.peek2(syn::Ident)
                    && state_definition.peek3(syn::Token![*])
                {
                    let _: syn::Token![*] = state_definition.parse()?;
                    let t_ident: syn::Ident = state_definition.parse()?;
                    let _: syn::Token![*] = state_definition.parse()?;
                    if t_ident.to_string() == "T" {
                        transient = true;
                    }
                }
            }
            short
        } else {
            // Check if there's a comma indicating an optional shortname or *T* marker
            if state_definition.peek(syn::Token![,]) {
                let _: syn::Token![,] = state_definition.parse()?;

                // Could be shortname or *T* marker
                if state_definition.peek(syn::Token![*])
                    && state_definition.peek2(syn::Ident)
                    && state_definition.peek3(syn::Token![*])
                {
                    let _: syn::Token![*] = state_definition.parse()?;
                    let t_ident: syn::Ident = state_definition.parse()?;
                    let _: syn::Token![*] = state_definition.parse()?;
                    if t_ident.to_string() == "T" {
                        transient = true;
                    }
                    state.state_name.clone()
                } else if state_definition.peek(syn::Ident) {
                    // It's a shortname
                    let short = state_definition.parse().map_err(|_| {
                        syn::Error::new(
                            state_definition.span(),
                            "Abacus Macro Error - State Parsing, shortname parsing failed.",
                        )
                    })?;
                    // Optional ", *T*" after shortname
                    if state_definition.peek(syn::Token![,]) {
                        let _: syn::Token![,] = state_definition.parse()?;
                        if state_definition.peek(syn::Token![*])
                            && state_definition.peek2(syn::Ident)
                            && state_definition.peek3(syn::Token![*])
                        {
                            let _: syn::Token![*] = state_definition.parse()?;
                            let t_ident: syn::Ident = state_definition.parse()?;
                            let _: syn::Token![*] = state_definition.parse()?;
                            if t_ident.to_string() == "T" {
                                transient = true;
                            }
                        }
                    }
                    short
                } else {
                    // No shortname, just use state name
                    state.state_name.clone()
                }
            } else {
                // In the absence of substates and comma, the shortname is just the state name
                state.state_name.clone()
            }
        };

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
    StateChange(State, Punctuated<syn::Path, syn::Token![,]>),
    StateChangeRW,
}

impl RegisterType {
    pub fn to_ident(&self) -> Ident {
        match self {
            RegisterType::ReadOnly => format_ident!("ReadOnly"),
            RegisterType::WriteOnly => format_ident!("WriteOnly"),
            RegisterType::ReadWrite => format_ident!("ReadWrite"),
            RegisterType::StateChange(_, _) => format_ident!("StateChange"),
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

                return Ok(RegisterType::StateChange(new_state, bitfield_instr));
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
            RegisterType::StateChange(state, instructions) => {
                assert_eq!(state.state_name.to_string(), "Active");
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
    fn test_state_definition_parse_simple() {
        let input = quote! { (Off) };
        let state_def: StateDefinition = syn::parse2(input).unwrap();

        assert_eq!(state_def.state.state_name.to_string(), "Off");
        assert_eq!(state_def.state_shortname.to_string(), "Off");
        assert!(!state_def.transient);
    }

    #[test]
    fn test_state_definition_parse_with_substates() {
        let input = quote! { (Active(Tx, Rx), ActiveTxRx) };
        let state_def: StateDefinition = syn::parse2(input).unwrap();

        assert_eq!(state_def.state.state_name.to_string(), "Active");
        assert_eq!(state_def.state_shortname.to_string(), "ActiveTxRx");
        assert_eq!(state_def.state.substates.len(), 2);
    }

    #[test]
    fn test_state_definition_missing_paren_error() {
        let input = quote! { Off };
        let result: syn::Result<StateDefinition> = syn::parse2(input);

        assert!(result.is_err());
    }

    #[test]
    fn test_state_definition_transient_true() {
        let input = quote! { (Loading, *T*) };
        let state_def: StateDefinition = syn::parse2(input).unwrap();

        assert_eq!(state_def.state.state_name.to_string(), "Loading");
        assert_eq!(state_def.state_shortname.to_string(), "Loading");
        assert!(state_def.transient);
    }

    #[test]
    fn test_state_definition_transient_false() {
        let input = quote! { (Off) };
        let state_def: StateDefinition = syn::parse2(input).unwrap();

        assert_eq!(state_def.state.state_name.to_string(), "Off");
        assert_eq!(state_def.state_shortname.to_string(), "Off");
        assert!(!state_def.transient);
    }

    #[test]
    fn test_state_definition_transient_with_substates() {
        let input = quote! { (Active(Tx, Rx), ActiveTxRx, *T*) };
        let state_def: StateDefinition = syn::parse2(input).unwrap();

        assert_eq!(state_def.state.state_name.to_string(), "Active");
        assert_eq!(state_def.state_shortname.to_string(), "ActiveTxRx");
        assert!(state_def.transient);
        assert_eq!(state_def.state.substates.len(), 2);
    }

    #[test]
    fn test_state_definition_invalid_transient_marker() {
        // Trailing tokens after closing paren cause parse error
        let input = quote! { (Loading) => [Active] *X* };
        let result: syn::Result<StateDefinition> = syn::parse2(input);

        assert!(result.is_err());
    }

    #[test]
    fn test_state_definition_incomplete_transient_marker() {
        let input = quote! { (Loading) };
        let state_def: StateDefinition = syn::parse2(input).unwrap();

        assert_eq!(state_def.state.state_name.to_string(), "Loading");
        assert!(!state_def.transient);
    }

    #[test]
    fn test_state_definition_transient_in_parentheses() {
        let input = quote! { (Loading, *T*) };
        let state_def: StateDefinition = syn::parse2(input).unwrap();

        assert_eq!(state_def.state.state_name.to_string(), "Loading");
        assert_eq!(state_def.state_shortname.to_string(), "Loading");
        assert!(state_def.transient);
    }

    #[test]
    fn test_state_definition_transient_with_custom_shortname() {
        let input = quote! { (Loading, CustomName, *T*) };
        let state_def: StateDefinition = syn::parse2(input).unwrap();

        assert_eq!(state_def.state.state_name.to_string(), "Loading");
        assert_eq!(state_def.state_shortname.to_string(), "CustomName");
        assert!(state_def.transient);
    }

    #[test]
    fn test_state_definition_substates_with_transient_in_parentheses() {
        let input = quote! { (Active(Tx, Rx), ActiveTxRx, *T*) };
        let state_def: StateDefinition = syn::parse2(input).unwrap();

        assert_eq!(state_def.state.state_name.to_string(), "Active");
        assert_eq!(state_def.state_shortname.to_string(), "ActiveTxRx");
        assert!(state_def.transient);
        assert_eq!(state_def.state.substates.len(), 2);
    }

    #[test]
    fn test_state_definition_simple_with_optional_shortname() {
        let input = quote! { (Loading, CustomName) };
        let state_def: StateDefinition = syn::parse2(input).unwrap();

        assert_eq!(state_def.state.state_name.to_string(), "Loading");
        assert_eq!(state_def.state_shortname.to_string(), "CustomName");
        assert!(!state_def.transient);
    }

    #[test]
    fn test_state_definition_invalid_transient_marker_in_parentheses() {
        // *X* is parsed but not recognized as transient (only *T* sets transient), so parse succeeds with transient=false
        let input = quote! { (Loading, *X*) };
        let state_def: StateDefinition = syn::parse2(input).unwrap();

        assert_eq!(state_def.state.state_name.to_string(), "Loading");
        assert!(!state_def.transient);
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
        );
        assert_eq!(state_change.to_ident().to_string(), "StateChange");
    }
}
