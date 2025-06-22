use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    hash::Hash,
};

use fabricator_vm::Span;
use thiserror::Error;

use crate::lexer::{Token, TokenKind};

#[derive(Debug, Error)]
pub enum MacroErrorKind {
    #[error("`#macro` must be at the beginning of a line")]
    TrailingMacro,
    #[error("bad or missing macro name, must be an identifier")]
    BadMacroName,
}

#[derive(Debug, Error)]
#[error("{kind}")]
pub struct MacroError {
    pub kind: MacroErrorKind,
    pub span: Span,
}

#[derive(Debug, Error)]
#[error("macro {macro_name:?} depends on itself recursively")]
pub struct RecursiveMacro<S> {
    pub macro_name: S,
}

pub struct Macros<S> {
    macros: HashMap<S, Vec<Token<S>>>,
}

impl<S> Default for Macros<S> {
    fn default() -> Self {
        Self {
            macros: HashMap::new(),
        }
    }
}

impl<S> Macros<S> {
    pub fn macro_names(&self) -> impl Iterator<Item = &S> + '_ {
        self.macros.keys()
    }
}

impl<S: Eq + Hash> Macros<S> {
    /// Extract macros from the given token list and store them.
    ///
    /// This wiill extract all `#macro NAME <TOKENS>` directives from the token list. Macros must
    /// be the first token following a newline, and the macro <TOKENS> list is interpreted up to
    /// the following newline or eof. All of the tokens that make up the macro are removed, not
    /// including the trailing newline or eof.
    ///
    /// If an error is encountered, the provided token buffer will be in an unspecified state.
    pub fn extract_macros(&mut self, tokens: &mut Vec<Token<S>>) -> Result<(), MacroError> {
        let mut token_iter = tokens.drain(..).peekable();
        let mut filtered_tokens = Vec::new();
        let mut prev_token_was_newline = true;

        while let Some(token) = token_iter.next() {
            if matches!(token.kind, TokenKind::Macro) {
                if !prev_token_was_newline {
                    return Err(MacroError {
                        kind: MacroErrorKind::TrailingMacro,
                        span: token.span,
                    });
                }

                match token_iter.next() {
                    Some(Token {
                        kind: TokenKind::Identifier(macro_name),
                        ..
                    }) => {
                        let mut macro_tokens = Vec::new();
                        loop {
                            match token_iter.peek() {
                                Some(t) => {
                                    if matches!(t.kind, TokenKind::EndOfStream | TokenKind::Newline)
                                    {
                                        break;
                                    } else {
                                        macro_tokens.push(token_iter.next().unwrap());
                                    }
                                }
                                None => break,
                            }
                        }

                        self.macros.insert(macro_name, macro_tokens);
                    }
                    t => {
                        return Err(MacroError {
                            kind: MacroErrorKind::BadMacroName,
                            span: t.map(|t| t.span).unwrap_or(Span::null()),
                        });
                    }
                }
            } else {
                prev_token_was_newline = matches!(token.kind, TokenKind::Newline);
                filtered_tokens.push(token);
            }
        }

        drop(token_iter);
        *tokens = filtered_tokens;

        Ok(())
    }

    pub fn find_macro<Q: ?Sized>(&self, name: &Q) -> Option<&[Token<S>]>
    where
        S: Borrow<Q>,
        Q: Hash + Eq,
    {
        Some(self.macros.get(name)?.as_slice())
    }
}

impl<S: Clone + Eq + Hash> Macros<S> {
    pub fn resolve_dependencies(&mut self) -> Result<(), RecursiveMacro<S>> {
        let mut dependent_stack = self.macros.keys().collect::<Vec<_>>();
        let mut resolving_dependencies = HashSet::new();
        let mut resolved_dependencies = HashSet::new();
        let mut resolve_order = Vec::new();

        'outer: loop {
            let Some(macro_name) = dependent_stack.pop() else {
                break;
            };

            if resolved_dependencies.contains(macro_name) {
                continue;
            }

            for token in &self.macros[macro_name] {
                if let TokenKind::Identifier(i) = &token.kind {
                    if self.macros.contains_key(i) && !resolved_dependencies.contains(i) {
                        if !resolving_dependencies.insert(macro_name) {
                            return Err(RecursiveMacro {
                                macro_name: macro_name.clone(),
                            });
                        }
                        dependent_stack.push(macro_name);
                        dependent_stack.push(i);
                        continue 'outer;
                    }
                }
            }

            resolved_dependencies.insert(macro_name);
            resolve_order.push(macro_name);
        }

        resolve_order.reverse();

        let mut resolved_macros: HashMap<S, Vec<Token<S>>> = HashMap::new();

        while let Some(macro_name) = resolve_order.pop() {
            let mut expanded_tokens = Vec::new();
            for token in &self.macros[&macro_name] {
                match &token.kind {
                    TokenKind::Identifier(i) if self.macros.contains_key(i) => {
                        expanded_tokens.extend_from_slice(&resolved_macros[i]);
                    }
                    _ => {
                        expanded_tokens.push(token.clone());
                    }
                }
            }

            resolved_macros.insert(macro_name.clone(), expanded_tokens);
        }

        self.macros = resolved_macros;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{lexer::Lexer, string_interner::StringInterner};

    struct SimpleInterner;

    impl StringInterner for SimpleInterner {
        type String = String;

        fn intern(&mut self, s: &str) -> Self::String {
            s.to_owned()
        }
    }

    #[test]
    fn test_macro_dependencies() {
        const SOURCE: &str = r#"
            #macro THREE TWO + ONE
            #macro ONE 1
            #macro FOUR TWO + TWO
            #macro TWO ONE + ONE
        "#;

        let mut tokens = Vec::new();
        Lexer::tokenize(SimpleInterner, SOURCE, &mut tokens).unwrap();

        let mut macros = Macros::default();

        macros.extract_macros(&mut tokens).unwrap();
        macros.resolve_dependencies().unwrap();

        let four = macros.find_macro("FOUR").unwrap();
        assert_eq!(
            four.iter().map(|t| t.kind.clone()).collect::<Vec<_>>(),
            [
                TokenKind::<String>::Integer(1),
                TokenKind::<String>::Plus,
                TokenKind::<String>::Integer(1),
                TokenKind::<String>::Plus,
                TokenKind::<String>::Integer(1),
                TokenKind::<String>::Plus,
                TokenKind::<String>::Integer(1),
            ]
        )
    }

    #[test]
    fn test_recursive_macros() {
        const SOURCE: &str = r#"
            #macro ONE TWO
            #macro TWO ONE
        "#;

        let mut tokens = Vec::new();
        Lexer::tokenize(SimpleInterner, SOURCE, &mut tokens).unwrap();

        let mut macros = Macros::default();

        macros.extract_macros(&mut tokens).unwrap();
        assert!(macros.resolve_dependencies().is_err());
    }
}
