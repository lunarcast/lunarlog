// @ts-ignore nearley import
import syntax from "./syntax.ne";
import { Grammar, Parser } from "nearley";

/** Generalized this in case I later want to create more than one parser from the same grammar */
const mkParser = (start: string) => {
  const grammar = Grammar.fromCompiled({ ...syntax, ParserStart: start });

  return (input: string) => {
    const parser = new Parser(grammar);

    parser.feed(input + "#eof");

    return parser.results[0];
  };
};

/** Parser a textul representation of a lunarlog compound */
export const parsePattern = mkParser("patternEof");
