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
  createBranch(name: string, argCount: number): T;
  editBranch(name: string, index: number): T;
}

export interface ForeignThumbail {
  thumbail: string;
  name: string;
  index: number;
}

export interface ForeignArguments {
  thumbails: ForeignStream<ForeignThumbail>;
}

export type ForeignAction = ADT<{
  createRule: {
    name: string;
  };
  createBranch: {
    name: string;
    argumentCount: number;
  };
  editBranch: {
    name: string;
    index: number;
  };
}>;

export interface InitialState {
  argumentCount: number;
  name: string;
}

interface MainArgs<T> {
  actions: ForeignStream<T>;
  initialState: InitialState;
}

const rawMain = (
  args: <T>(constructors: ForeignConstructors<T>) => MainArgs<T>
): [ForeignArguments, Effect<void>] => {
  const tuple = purescript.main(args)();

  return [tuple.value0, tuple.value1];
};

export const main = (
  initialState: InitialState,
  stream: Stream<ForeignAction>
) =>
  rawMain((constructors) => ({
    initialState,
    actions: (emit) => () =>
      stream((tsAction) =>
        emit(
          matchI(tsAction)({
            createRule: ({ name }) => constructors.createRule(name),
            createBranch: ({ name, argumentCount }) =>
              constructors.createBranch(name, argumentCount),
            editBranch: ({ name, index }) =>
              constructors.editBranch(name, index),
          })
        )()
      ),
  }));
