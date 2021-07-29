@{%
const { lexer, convertToken, convertTokenId } = require("./lexer.ts")
const { var_, natural, constructor, list, pattern } = require("./ast.ts")
%}

@lexer lexer

pattern -> patternName _ (term _ {% id %}):* {% ([name, _, arguments_]) => constructor(name, arguments_) %}

term 
    -> natural {% ([a]) => natural(a) %} 
    | identifier {% ([a]) => var_(a) %} 
    | patternName {% ([a]) => pattern(constructor(a, [])) %}
    | "(" _ termInParens _ ")" {% i => i[2] %}
    | ("[" {% convertTokenId %}) _ (term _ {% id %}):? ("," _ term _ {% a => a[2] %}):* ("|" _ term _ {% a => a[2] %}):? ("]" {% convertTokenId %}) 
        {% ([open,,first, rest, tail, close]) => list(
            { start: open.span.start, end: close.span.end }, 
            first === null ? [] : [first, ...rest],
            tail
        ) %}

termInParens
    -> pattern {% ([c]) => pattern(c) %} 
    | term {% id %}

# Lexing
identifier -> %identifier {% convertTokenId %}
patternName -> %constructor {% convertTokenId %}
natural -> %natural {% convertTokenId %}

_ -> %whitespace:?
