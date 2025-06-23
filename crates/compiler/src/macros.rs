use std::{
    borrow::Borrow,
    collections::{HashMap, hash_map},
    hash::Hash,
};

use fabricator_util::index_containers::{IndexMap, IndexSet};
use fabricator_vm::Span;
use gc_arena::Collect;
use thiserror::Error;

use crate::lexer::{Token, TokenKind};

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct Macro<S> {
    pub name: S,
    pub tokens: Vec<Token<S>>,
    pub span: Span,
}

#[derive(Debug, Error)]
pub enum MacroErrorKind {
    #[error("`#macro` must be at the beginning of a line")]
    TrailingMacro,
    #[error("bad or missing macro name, must be an identifier")]
    BadMacroName,
    #[error("macro name is a duplicate of macro #{0}")]
    DuplicateMacro(usize),
}

#[derive(Debug, Error)]
#[error("{kind}")]
pub struct MacroError {
    pub kind: MacroErrorKind,
    pub span: Span,
}

#[derive(Debug, Error)]
#[error("macro #{0} depends on itself recursively")]
pub struct RecursiveMacro(pub usize);

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct MacroSet<S> {
    macros: Vec<Macro<S>>,
    macro_dict: HashMap<S, usize>,
}

impl<S> Default for MacroSet<S> {
    fn default() -> Self {
        Self {
            macros: Vec::new(),
            macro_dict: HashMap::new(),
        }
    }
}

impl<S> MacroSet<S> {
    pub fn new() -> Self {
        Self::default()
    }

    /// The current count of extracted macros.
    ///
    /// Each macro is assigned a sequential index for identification starting from zero. Checking
    /// the current macro count can be used to determine which macros are extracted from which calls
    /// to [`MacroBuilder::extract_macros`].
    pub fn len(&self) -> usize {
        self.macros.len()
    }

    /// Get an extracted macro.
    pub fn get(&self, index: usize) -> Option<&Macro<S>> {
        self.macros.get(index)
    }
}

impl<S: Eq + Hash> MacroSet<S> {
    /// Find a macro index by macro name.
    pub fn find<Q: ?Sized>(&self, name: &Q) -> Option<usize>
    where
        S: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.macro_dict.get(name).copied()
    }
}

impl<S: Clone + Eq + Hash> MacroSet<S> {
    /// Extract macros from the given token list and store them.
    ///
    /// This wiill extract all `#macro NAME <TOKENS>` directives from the token list. Macros must
    /// be the first token following a newline, and the macro <TOKENS> list is interpreted up to
    /// the following newline or eof. All of the tokens that make up the macro are removed, not
    /// including the trailing newline or eof.
    ///
    /// If an error is encountered, the provided token buffer will be in an unspecified state.
    pub fn extract(&mut self, tokens: &mut Vec<Token<S>>) -> Result<(), MacroError> {
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

                let mut macro_span = token.span;

                match token_iter.next() {
                    Some(Token {
                        kind: TokenKind::Identifier(macro_name),
                        span: name_span,
                    }) => {
                        macro_span = macro_span.combine(name_span);

                        let mut macro_tokens = Vec::new();
                        loop {
                            match token_iter.peek() {
                                Some(t) => {
                                    if matches!(t.kind, TokenKind::EndOfStream | TokenKind::Newline)
                                    {
                                        break;
                                    } else {
                                        let token = token_iter.next().unwrap();
                                        macro_span = macro_span.combine(token.span);
                                        macro_tokens.push(token);
                                    }
                                }
                                None => break,
                            }
                        }

                        match self.macro_dict.entry(macro_name.clone()) {
                            hash_map::Entry::Occupied(occupied) => {
                                return Err(MacroError {
                                    kind: MacroErrorKind::DuplicateMacro(*occupied.get()),
                                    span: macro_span,
                                });
                            }
                            hash_map::Entry::Vacant(vacant) => {
                                vacant.insert(self.macros.len());
                            }
                        }

                        self.macros.push(Macro {
                            name: macro_name,
                            tokens: macro_tokens,
                            span: macro_span,
                        });
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

    /// Resolve all inter-macro dependencies.
    ///
    /// After a successful call here, macros are guaranteed to be fully recursively expanded. All
    /// instances of `Token::Identifier` that reference another macro in the set will be replaced
    /// with the fully expanded macro that the identifier references.
    ///
    /// Will return `Err` if any macro depends on itself recursively.
    pub fn resolve_dependencies(&mut self) -> Result<(), RecursiveMacro> {
        // Determine a proper macro evaluation order.
        //
        // Add all of the macro indexes we need to evaluate to a stack.
        //
        // Take the top entry off of the stack and check to see if it has any un-evaluated
        // dependencies. If it does, then push the popped macro back onto the stack, followed
        // by all of its un-evaluated dependencies. Otherwise if the macro has no un-evaluated
        // dependencies, then it can be evaluated next.
        //
        // We also keep track of in-progress *evaluating* dependencies (un-evaluated dependencies
        // that we have encountered before and pushed onto our stack but are not yet evaluated). If
        // we encounter one of these dependencies more than once without it becoming evaluated, then
        // we know we have a recursive macro.

        let mut eval_stack = (0..self.macros.len()).collect::<Vec<_>>();
        let mut evaluating_dependencies = IndexSet::new();
        let mut evaluated_dependencies = IndexSet::new();
        let mut eval_order = Vec::new();

        'outer: loop {
            let Some(macro_index) = eval_stack.pop() else {
                break;
            };

            if evaluated_dependencies.contains(macro_index) {
                continue;
            }

            let makro = &self.macros[macro_index];
            for token in &makro.tokens {
                if let TokenKind::Identifier(i) = &token.kind {
                    if let Some(&ind) = self.macro_dict.get(i) {
                        if !evaluated_dependencies.contains(ind) {
                            if !evaluating_dependencies.insert(macro_index) {
                                return Err(RecursiveMacro(macro_index));
                            }
                            eval_stack.push(macro_index);
                            eval_stack.push(ind);
                            continue 'outer;
                        }
                    }
                }
            }

            evaluated_dependencies.insert(macro_index);
            eval_order.push(macro_index);
        }

        // Once we have a known-good evaluation order, we can evaluate our interdependent macros in
        // this order and all references should be present.

        let mut resolved_macros = IndexMap::<Macro<S>>::new();

        for macro_index in eval_order {
            let mut expanded_tokens = Vec::new();
            let makro = &self.macros[macro_index];
            for token in &makro.tokens {
                match &token.kind {
                    TokenKind::Identifier(i) if self.macro_dict.contains_key(i) => {
                        let ind = self.macro_dict[i];
                        expanded_tokens.extend_from_slice(
                            &resolved_macros
                                .get(ind)
                                .expect("bad macro evaluation order")
                                .tokens,
                        );
                    }
                    _ => {
                        expanded_tokens.push(token.clone());
                    }
                }
            }

            resolved_macros.insert(
                macro_index,
                Macro {
                    name: makro.name.clone(),
                    tokens: expanded_tokens,
                    span: makro.span,
                },
            );
        }

        self.macros = (0..self.macros.len())
            .map(|i| resolved_macros.remove(i).unwrap())
            .collect();
        Ok(())
    }

    /// Expand all macros in the given token list.
    pub fn expand(&mut self, tokens: &mut Vec<Token<S>>) {
        let mut expanded_tokens = Vec::new();

        for token in tokens.drain(..) {
            let macro_index = if let TokenKind::Identifier(i) = &token.kind {
                self.find(i)
            } else {
                None
            };

            if let Some(macro_index) = macro_index {
                expanded_tokens.extend_from_slice(&self.macros[macro_index].tokens);
            } else {
                expanded_tokens.push(token);
            }
        }

        *tokens = expanded_tokens;
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

        let mut macros = MacroSet::new();

        macros.extract(&mut tokens).unwrap();
        macros.resolve_dependencies().unwrap();

        assert_eq!(
            macros
                .get(macros.find("FOUR").unwrap())
                .unwrap()
                .tokens
                .iter()
                .map(|t| t.kind.clone())
                .collect::<Vec<_>>(),
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

        let mut macros = MacroSet::default();

        macros.extract(&mut tokens).unwrap();
        assert!(macros.resolve_dependencies().is_err());
    }
}
