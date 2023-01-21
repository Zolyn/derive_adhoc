//! Core types and traits for parsing and expansion
//!
//! Also re-exports the names that the implementation wants.

pub use crate::prelude::*;

pub use crate::boolean::*;
pub use crate::repeat::*;
pub use crate::syntax::*;

pub(crate) use crate::paste;

/// Context during expansion
///
/// References the driver, and digested information about it.
/// Also represents where in the driver we are,
/// including repetition context.
#[derive(Debug, Clone)]
pub struct Context<'c> {
    pub top: &'c syn::DeriveInput,
    pub tattrs: &'c PreprocessedAttrs,
    pub variant: Option<&'c WithinVariant<'c>>,
    pub field: Option<&'c WithinField<'c>>,
    pub pvariants: &'c [PreprocessedVariant<'c>],
}

#[derive(Debug, Clone)]
pub struct PreprocessedVariant<'f> {
    pub fields: &'f syn::Fields,
    pub pattrs: PreprocessedAttrs,
    pub pfields: Vec<PreprocessedField>,
}

#[derive(Debug, Clone)]
pub struct PreprocessedField {
    pub pattrs: PreprocessedAttrs,
}

pub type PreprocessedAttr = syn::Meta;
pub type PreprocessedAttrs = Vec<PreprocessedAttr>;

#[derive(Debug, Clone)]
pub struct WithinVariant<'c> {
    pub variant: Option<&'c syn::Variant>,
    pub fields: &'c syn::Fields,
    pub pattrs: &'c PreprocessedAttrs,
    pub pfields: &'c [PreprocessedField],
}

#[derive(Debug, Clone)]
pub struct WithinField<'c> {
    pub field: &'c syn::Field,
    pub pfield: &'c PreprocessedField,
    pub index: u32,
}

/// Surrounding lexical context during parsing
///
/// This is the kind of lexical context a piece of a template appears in.
/// It is implemented for
///  * Types that represent an expansion output `ExpansionOutput`;
///    in this case, the lexical context is one where
///    the expansion is accumulated in this type.
///  * Places where template substitution syntax `${keyword }`
///    appears but where no output will be generated (eg, within
///    the condition of `${if }`.
///
/// The associated types are either `Void` or `()`.
/// They appears within the variants of `SubstDetails`,
/// causing inapplicable variants to be eliminated.
///
/// Because a variant is only inhabited if all of its fields are,
/// the conditions are effectively ANDed.
/// So the "default" value (for context that don't have an opnion)
/// is inhabitedness `()`.
///
/// Each type has an associated constructure,
/// used during parsing.
/// So this generates a parse error are parse time,
/// if a construct appears in the wrong place.
pub trait SubstParseContext {
    /// Uninhabited iff this lexical context is within `${paste }`
    type NoPaste: Debug + Copy + Sized;
    /// Uninhabited iff this lexical context is within `${case }`
    type NoCase: Debug + Copy + Sized;
    /// Uninhabited iff this lexical context is within a condition.
    type NoBool: Debug + Copy + Sized;
    /// Uninhabited unless lexical context allows other than a single subst
    ///
    /// Used for `${case }`; could be used in other places where we
    /// accept the `${...}` syntax for an expansion, but want to
    /// reject repetitions, `${if }`, and so on.
    type NoNonterminal: Debug + Copy + Sized;
    /// Uninhabited unless this lexical context is within a condition.
    type BoolOnly: Debug + Copy + Sized;

    fn no_paste(span: &impl Spanned) -> syn::Result<Self::NoPaste>;
    fn no_case(span: &impl Spanned) -> syn::Result<Self::NoCase>;
    fn no_bool(span: &impl Spanned) -> syn::Result<Self::NoBool>;
    fn no_nonterminal(span: &impl Spanned)
        -> syn::Result<Self::NoNonterminal>;

    fn bool_only(span: &impl Spanned) -> syn::Result<Self::BoolOnly> {
        Err(span.error(
            "derive-adhoc keyword is a condition - not valid as an expansion",
        ))
    }
}

/// Expansion output accumulator, for a template lexical context
///
/// Each template lexical context has a distinct type which
///  * Represents the lexical context
///  * If that lexical context generates expansions,
///    accumulates the expansion.  That's what this trait is.
///
/// The methods are for accumulating various kinds of things
/// that can be found in templates, or result from template expansion.
///
/// The accumulating type (`Self` might be accumulating
/// tokens ([`TokenStream`]) or strings ([`paste::Items`)).
pub trait ExpansionOutput: SubstParseContext {
    /// Some kind of thing, whose `Display` impl is to be used
    ///
    /// This could be an `str` for example.
    /// This is *not* suitable for `TokenTree::Literal` or `syn::Lit`
    /// because their `Display` impls produce the version with `" "`.
    fn push_display<I: Display + Spanned + ToTokens>(&mut self, lit: &I);

    /// An identifier (or fragment of one)
    fn push_ident<I: quote::IdentFragment + Spanned + ToTokens>(
        &mut self,
        ident: &I,
    );

    /// An identifier path
    ///
    /// Consisting of some prefix tokens (perhaps a scoping path),
    /// the actual identifer,
    /// and some suffix tokens (perhaps generics).
    ///
    /// `template_entry_span` is the span of the part of the template
    /// which expanded into this identifier path.
    fn push_idpath<A, B>(
        &mut self,
        template_entry_span: Span,
        pre: A,
        ident: &syn::Ident,
        post: B,
    ) where
        A: FnOnce(&mut TokenAccumulator),
        B: FnOnce(&mut TokenAccumulator);

    /// [`syn::Lit`](enum@syn::Lit)
    ///
    /// This is its own method because `syn::Lit` is not `Display`,
    /// and we don't want to unconditionally turn it into a string
    /// before retokenising it.
    fn push_syn_lit(&mut self, v: &syn::Lit);

    /// [`syn::Type`]
    fn push_syn_type(&mut self, te_span: Span, v: &syn::Type);

    /// Some other substitution which generates tokens
    ///
    /// Not supported within `${paste }`.
    /// The `NoPaste` parameter makes this method unreachable
    /// when expanding within `${paste }`;
    /// or to put it another way,
    /// it ensures that such an attempt would have been rejected
    /// during template parsing.
    fn push_other_subst<S, F>(
        &mut self,
        np: &Self::NoPaste,
        _: &S,
        f: F,
    ) -> syn::Result<()>
    where
        S: Spanned,
        F: FnOnce(&mut TokenAccumulator) -> syn::Result<()>;

    /// A substitution which can only be used within a boolean.
    ///
    /// This cannot be expanded, so this function must be unreachable.
    ///
    /// The requirement to implement it involves demonstrating that
    /// either self, or BoolOnly, is uninhabited.
    ///
    /// And, then, this function can be called in expansion contexts
    /// to handle uninhabited variants.
    fn expand_bool_only(&mut self, bool_only: &Self::BoolOnly) -> !;

    /// Expand a `${paste }`
    fn expand_paste(
        &mut self,
        np: &Self::NoPaste,
        ctx: &Context,
        span: Span,
        paste_body: &Template<paste::Items>,
    ) -> syn::Result<()>;

    /// Expand a `${case }`
    fn expand_case(
        &mut self,
        np: &Self::NoCase,
        case: paste::ChangeCase,
        ctx: &Context,
        span: Span,
        paste_body: &Subst<paste::Items<paste::WithinCaseContext>>,
    ) -> syn::Result<()>;

    /// Note that an error occurred
    ///
    /// This must arrange to
    /// (eventually) convert it using `into_compile_error`
    /// and emit it somewhere appropriate.
    fn record_error(&mut self, err: syn::Error);

    /// Convenience method for noting an error with span and message
    fn write_error<S: Spanned, M: Display>(&mut self, span: &S, message: M) {
        self.record_error(span.error(message));
    }
}

/// Convenience trait providing `item.expand()`
///
/// Implementations of this are often specific to the [`ExpansionOutput`].
///
/// Having this as a separate trait,
/// rather than hanging it off `ExpansionOutput`,
/// makes the expansion method more convenient to call.
///
/// It also avoids having to make all of these expansion methods
/// members of the `ExpansionOutput` trait.
pub trait Expand<O> {
    fn expand(&self, ctx: &Context, out: &mut O) -> syn::Result<()>;
}
/// Convenience trait providing `fn expand(self)`, infallible version
///
/// Some of our `expand` functions always record errors
/// within the output accumulator
/// and therefore do not need to return them.
pub trait ExpandInfallible<O> {
    fn expand(&self, ctx: &Context, out: &mut O);
}

/// Accumulates tokens, or errors
///
/// We collect all the errors, and if we get an error, don't write
/// anything out.
/// This is because `compile_error!` (from `into_compile_error`)
/// only works in certain places in Rust syntax (!)
#[derive(Debug)]
pub struct TokenAccumulator(Result<TokenStream, syn::Error>);

impl Default for TokenAccumulator {
    fn default() -> Self {
        TokenAccumulator(Ok(TokenStream::new()))
    }
}

impl TokenAccumulator {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn with_tokens<R>(
        &mut self,
        f: impl FnOnce(&mut TokenStream) -> R,
    ) -> Option<R> {
        self.0.as_mut().ok().map(f)
    }
    pub fn write_tokens(&mut self, t: impl ToTokens) {
        self.with_tokens(|out| t.to_tokens(out));
    }
    pub fn tokens(self) -> syn::Result<TokenStream> {
        self.0
    }
}

impl SubstParseContext for TokenAccumulator {
    type NoPaste = ();
    type NoBool = ();
    type NoCase = ();
    type NoNonterminal = ();
    fn no_bool(_: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }
    fn no_paste(_: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }
    fn no_case(_: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }
    fn no_nonterminal(_: &impl Spanned) -> syn::Result<()> {
        Ok(())
    }

    type BoolOnly = Void;
}

impl ExpansionOutput for TokenAccumulator {
    fn push_display<L: Display + Spanned + ToTokens>(&mut self, lit: &L) {
        self.write_tokens(lit)
    }
    fn push_ident<I: quote::IdentFragment + Spanned + ToTokens>(
        &mut self,
        ident: &I,
    ) {
        self.write_tokens(ident)
    }
    fn push_idpath<A, B>(
        &mut self,
        _te_span: Span,
        pre: A,
        ident: &syn::Ident,
        post: B,
    ) where
        A: FnOnce(&mut TokenAccumulator),
        B: FnOnce(&mut TokenAccumulator),
    {
        pre(self);
        self.write_tokens(ident);
        post(self);
    }
    fn push_syn_lit(&mut self, lit: &syn::Lit) {
        self.write_tokens(lit);
    }
    fn push_syn_type(&mut self, _te_span: Span, ty: &syn::Type) {
        self.write_tokens(ty);
    }
    fn push_other_subst<S, F>(
        &mut self,
        _no_paste: &(),
        _: &S,
        f: F,
    ) -> syn::Result<()>
    where
        S: Spanned,
        F: FnOnce(&mut TokenAccumulator) -> syn::Result<()>,
    {
        f(self)
    }

    fn expand_paste(
        &mut self,
        _no_paste: &(),
        ctx: &Context,
        span: Span,
        paste_body: &Template<paste::Items>,
    ) -> syn::Result<()> {
        let mut items = paste::Items::new(span);
        paste_body.expand(ctx, &mut items);
        items.assemble(self)
    }
    fn expand_case(
        &mut self,
        _no_case: &(),
        case: paste::ChangeCase,
        ctx: &Context,
        span: Span,
        paste_body: &Subst<paste::Items<paste::WithinCaseContext>>,
    ) -> syn::Result<()> {
        let mut items = paste::Items::new_case(span, case);
        paste_body.expand(ctx, &mut items)?;
        items.assemble(self)
    }
    fn expand_bool_only(&mut self, bool_only: &Self::BoolOnly) -> ! {
        void::unreachable(*bool_only)
    }

    fn record_error(&mut self, err: syn::Error) {
        if let Err(before) = &mut self.0 {
            before.combine(err);
        } else {
            self.0 = Err(err)
        }
    }
}
