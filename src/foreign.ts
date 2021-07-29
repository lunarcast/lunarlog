import { ADT, match, matchI } from "ts-adt";
// @ts-ignore
import * as purescript from "../output/Main";
import { Constructor } from "./parser/ast";
import { Effect, ForeignStream, Stream } from "./Stream";

export const renderPatternToImage = (
  name: string,
  arguments_: number
): Promise<string> => {
  return purescript.renderPatternToImage({ name, arguments: arguments_ })()
    .value0;
};

interface ForeignConstructors<T> {
  createRule(name: string): T;
  addNode(name: string, argCount: number): T;
  editBranch(name: string, branchId: number): T;
  deleteBranch(name: string, branchId: number): T;
  togglePointerEvents(shouldGetEnabled: boolean): T;
  evaluateQuery(query: Constructor): T;
  createBranch(
    name: string,
    branchId: number,
    pattern: { name: string; argumentCount: number }
  ): T;
}

export type Substitution = Array<{
  name: string;
  solution: string;
}>;

export interface ForeignArguments {
  queryResults: ForeignStream<Array<Substitution>>;
}

export type ForeignAction = ADT<{
  createBranch: {
    name: string;
    argumentCount: number;
    branchId: number;
  };
  editBranch: {
    name: string;
    branchId: number;
  };
  deleteBranch: {
    name: string;
    branchId: number;
  };
  addNode: {
    name: string;
    argumentCount: number;
  };
  togglePointerEvents: {
    shouldGetEnabled: boolean;
  };
  evaluateQuery: {
    query: Constructor;
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
            createBranch: ({ name, argumentCount, branchId: index }) =>
              constructors.createBranch(name, index, { name, argumentCount }),
            editBranch: ({ name, branchId: index }) =>
              constructors.editBranch(name, index),
            addNode: ({ name, argumentCount }) =>
              constructors.addNode(name, argumentCount),
            togglePointerEvents: ({ shouldGetEnabled }) =>
              constructors.togglePointerEvents(shouldGetEnabled),
            deleteBranch: ({ name, branchId }) =>
              constructors.deleteBranch(name, branchId),
            evaluateQuery: ({ query }) => constructors.evaluateQuery(query),
          })
        )()
      ),
  }));
