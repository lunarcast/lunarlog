/** Js representation for purescript variants */
export type Variant<T extends object> = {
  [key in keyof T]: { type: key; value: T[key] };
}[keyof T];

// ========== Lexing stuff
export type SourcePosition = { line: number; column: number };
export type SourceSpan = { start: SourcePosition; end: SourcePosition };
export type Token = { type: string; text: string; span: SourceSpan };

export type WithSpan<T> = { value: T; span: SourceSpan };

// ========== Actual dsl
export type Constructor = {
  name: WithSpan<string>;
  arguments: Term[];
};

export type Term = Variant<{
  var: WithSpan<string>;
  natural: WithSpan<number>;
  pattern: Constructor;
  list: {
    span: SourceSpan;
    elements: Term[];
    tail: Term | null;
  };
}>;

// ========== Constructors
export const var_ = (tok: Token): Term => ({
  type: "var",
  value: {
    span: tok.span,
    value: tok.text,
  },
});

export const natural = (tok: Token): Term => ({
  type: "natural",
  value: {
    span: tok.span,
    value: parseInt(tok.text),
  },
});

export const list = (
  span: SourceSpan,
  elements: Term[],
  tail?: Term
): Term => ({
  type: "list",
  value: {
    span,
    elements,
    tail: tail === undefined ? null : tail,
  },
});

export const constructor = (
  nameToken: Token,
  arguments_: Term[]
): Constructor => ({
  name: { span: nameToken.span, value: nameToken.text },
  arguments: arguments_,
});

export const pattern = (constructor: Constructor): Term => ({
  type: "pattern",
  value: constructor,
});
