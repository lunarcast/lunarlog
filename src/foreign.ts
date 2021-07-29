import { ADT, match, matchI } from "ts-adt";
// @ts-ignore
import * as purescript from "../output/Main";
import { Effect, ForeignStream, Stream } from "./Stream";

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

interface ForeignConstructors<T> {
  createRule(name: string): T;
  createBranch(
    name: string,
    index: number,
    pattern: { name: string; argumentCount: number }
  ): T;
  addNode(name: string, argCount: number): T;
  editBranch(name: string, index: number): T;
}

export interface ForeignArguments {}

export type ForeignAction = ADT<{
  createBranch: {
    name: string;
    argumentCount: number;
    index: number;
  };
  editBranch: {
    name: string;
    index: number;
  };
  addNode: {
    name: string;
    argumentCount: number;
  };
}>;

interface MainArgs<T> {
  actions: ForeignStream<T>;
}

const rawMain = (
  args: <T>(constructors: ForeignConstructors<T>) => MainArgs<T>
): [ForeignArguments, Effect<void>] => {
  const tuple = purescript.main(args)();

  return [tuple.value0, tuple.value1];
};

export const main = (stream: Stream<ForeignAction>) =>
  rawMain((constructors) => ({
    actions: (emit) => () =>
      stream((tsAction) =>
        emit(
          matchI(tsAction)({
            createBranch: ({ name, argumentCount, index }) =>
              constructors.createBranch(name, index, { name, argumentCount }),
            editBranch: ({ name, index }) =>
              constructors.editBranch(name, index),
            addNode: ({ name, argumentCount }) =>
              constructors.addNode(name, argumentCount),
          })
        )()
      ),
  }));
