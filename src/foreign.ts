// @ts-ignore
import * as purescript from "../output/Main";

export const renderPatternToImage = (
  name: string,
  arguments_: number
): Promise<string> => {
  console.log(
    purescript.renderPatternToImage({ name, arguments: arguments_ })()
  );

  return purescript.renderPatternToImage({ name, arguments: arguments_ })()
    .value0;
};
