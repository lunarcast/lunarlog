import "../styles/editor.scss";
import { useState, useEffect, useCallback } from "preact/hooks";
import { Button, Input } from "./Input";
import { ComponentChild } from "preact";
import { Spacing } from "./Spacing";
import { capitalize, capitalizeWord } from "./helpers";
import { ForeignAction, Substitution } from "../foreign";
import { useImmer } from "use-immer";
import { useKey, useBoolean, useCounter } from "react-use";
import { Icon } from "./Icon";
import type { ADT } from "ts-adt";
import { parsePattern } from "../parser/parser";
import * as Ast from "../parser/ast";
import { Stream, useStream } from "../Stream";
import { div } from "@thi.ng/matrices";

// ========== Types
type BranchId = number;

interface Rule {
  branches: Array<BranchId>;
}

type NodeMemory = Record<string, number>;
type RuleMemory = Record<string, Rule>;

type ParsedQuery = ADT<{
  success: {
    result: Ast.Constructor;
  };
  failure: {
    message: string;
  };
}>;

interface EditorPaneProps {
  title: string;
  disabled?: string;
  children: ComponentChild;
}

interface NodeListProps {
  nodes: Array<[string, number]>;
  disabled?: boolean;
  use(name: string): void;
}

interface NodeProps {
  name: string;
  argumentCount: number;
  use(): void;
}

interface RuleListProps {
  rules: RuleMemory;
  disabled?: boolean;
  selectedBranch: null | [string, number];
  createBranch(name: string): void;
  select(name: string, branchId: number): void;
  edit(name: string, branchId: number): void;
  delete(name: string, branchId: number): void;
}

interface BranchListProps {
  branches: Array<BranchId>;
  name: string;
  selectedBranch: null | number;
  createBranch(): void;
  select(branchId: number): void;
  edit(branchId: number): void;
  delete(branchId: number): void;
}

interface RuleBranchProps {
  name: string;
  isSelected: boolean;
  select(): void;
  edit(): void;
  delete(): void;
}

interface CreateCompoundProps {
  isTaken(name: string): boolean;
  create(name: string, argumentCount: number): void;
  productName: string;
  allowUpdates?: boolean;
  disabled?: boolean;
}

interface EditorProps {
  emit(action: ForeignAction): void;
  queryResults: Stream<Array<Substitution>>;
}

interface EditQueryProps {
  disabled?: boolean;
  queryResults: Stream<Array<Substitution>>;
  evaluate(query: Ast.Constructor): void;
}

interface QueryResultProps {
  substitutions: Array<Substitution>;
}

// ========== Constants
const nodeAdjective = [
  "interesting",
  "intriguing",
  "aweomse",
  "favorite",
  "cool",
  "important",
  "special",
  "unique",
  "original",
  "creative",
];

// ========== Components
const EditorPane = ({ children, title, disabled }: EditorPaneProps) => {
  return (
    <div
      className={["editor__pane", disabled && "editor__pane--disabled"]
        .filter((a) => a)
        .join(" ")}
    >
      <div className="editor__pane-container">
        <header className="editor__pane-title">{title}</header>
        <main className="editor__pane-body">{children}</main>
      </div>

      {disabled && (
        <div className="editor__pane-disabled-overlay">
          <span className="editor__pane-disabled-reason">{disabled}</span>{" "}
        </div>
      )}
    </div>
  );
};

const Node = ({ argumentCount, name, use }: NodeProps) => {
  return (
    <div className="node" onClick={use}>
      <div className="node__name">{name}</div>
      <div className="node__argument-count">{argumentCount}</div>
    </div>
  );
};

const EmptyNodeList = () => {
  return (
    <div className="editor__pane-empty-node-list">
      <p>You haven't created any node yet!</p>
    </div>
  );
};

// The node list panel
const NodeList = ({ nodes, disabled, use }: NodeListProps) => {
  return (
    <EditorPane
      title="Use node"
      disabled={
        disabled ? "Cannot use nodes without creating a rule first" : undefined
      }
    >
      <div id="node-list">
        {nodes.map(([name, argumentCount]) => (
          <Node
            name={name}
            key={name}
            use={() => use(name)}
            argumentCount={argumentCount}
          />
        ))}
      </div>
      {nodes.length === 0 && <EmptyNodeList />}
    </EditorPane>
  );
};

// The preview for a rule
const RuleBranch = ({
  edit,
  select,
  delete: deleteBranch,
  isSelected,
}: RuleBranchProps) => {
  return (
    <div
      className={["rule__branch", isSelected && "rule__branch--selected"].join(
        " "
      )}
      onClick={select}
    >
      <Icon onClick={edit}>edit</Icon>
      <Icon onClick={deleteBranch}>delete</Icon>
    </div>
  );
};

// The rule list panel
const BranchList = ({
  branches,
  name,
  createBranch,
  selectedBranch,
  delete: deleteBranch,
  edit,
  select,
}: BranchListProps) => {
  return (
    <div className="rule">
      <div className="rule__name">{capitalizeWord(name)}</div>
      <div className="rule__branches">
        {branches.map((branchId) => (
          <RuleBranch
            isSelected={branchId === selectedBranch}
            name={name}
            key={branchId}
            select={() => select(branchId)}
            edit={() => edit(branchId)}
            delete={() => deleteBranch(branchId)}
          />
        ))}
        <Button onClick={createBranch}>
          <Icon>add</Icon>
        </Button>
      </div>
    </div>
  );
};

const RuleList = ({
  rules,
  disabled,
  createBranch,
  selectedBranch,
  edit,
  select,
  delete: deleteBranch,
}: RuleListProps) => {
  return (
    <EditorPane
      title="Rules"
      disabled={disabled ? "No rules found" : undefined}
    >
      {Object.entries(rules).map(([name, branches]) => (
        <BranchList
          selectedBranch={
            selectedBranch?.[0] === name ? selectedBranch[1] : null
          }
          createBranch={() => createBranch(name)}
          name={name}
          key={name}
          branches={branches.branches}
          edit={(branchId) => edit(name, branchId)}
          select={(branchId) => select(name, branchId)}
          delete={(branchId) => deleteBranch(name, branchId)}
        />
      ))}
    </EditorPane>
  );
};

// Used for the create node and create rule components
const CreateCompound = ({
  isTaken,
  create: createNode,
  productName,
  allowUpdates,
  disabled,
}: CreateCompoundProps) => {
  const [name, setName] = useState("");
  const [argCount, setArgCount] = useState(2);
  let error: null | string = null;

  if (isTaken(name) && !allowUpdates) {
    error = `Name "${name}" is already taken`;
  }

  const resetInputs = () => {
    const adjective =
      nodeAdjective[Math.floor(Math.random() * nodeAdjective.length)];
    setName(`My ${adjective} ${productName}`);
    setArgCount(2);
  };

  useEffect(() => {
    resetInputs();
  }, []);

  const action = allowUpdates && isTaken(name) ? "Update" : "Create";

  return (
    <EditorPane
      title={`Create ${productName}`}
      disabled={
        disabled
          ? "Cannot create nodes without creating a rule first"
          : undefined
      }
    >
      <div id="create-node">
        <Input
          value={name}
          label={`${capitalize(productName)} name`}
          setValue={setName}
        />
        <Input
          type="number"
          value={argCount}
          label="Argument count"
          setValue={(c) => setArgCount(Math.max(0, Math.floor(c)))}
        />

        {error && <div id="create-node__error">{error}</div>}

        <Spacing />

        <div id="create-node__button-container">
          <Button
            disabled={!!error}
            onClick={() => {
              if (error === null) {
                createNode(name, argCount);
                resetInputs();
              }
            }}
          >
            {action} {productName}
          </Button>
        </div>
      </div>
    </EditorPane>
  );
};

const QueryResult = (props: QueryResultProps) => {
  if (props.substitutions.length === 0) {
    return (
      <div className="query__result-container">
        <div className="query__no-solution">No solutions found</div>
      </div>
    );
  }

  return (
    <div className="query__result-container">
      <div class="query__result-row">
        {props.substitutions[0].map(({ name }) => (
          <div key={name}>{name}</div>
        ))}
      </div>
      {props.substitutions.map((substitution, index) => (
        <div class="query__result-row" key={index}>
          {substitution.map(({ name, solution }) => (
            <div key={name}>{solution}</div>
          ))}
        </div>
      ))}
    </div>
  );
};

const EditQuery = (props: EditQueryProps) => {
  const [query, setQuery] = useState("");
  const [parsingResult, setParsingResult] = useState<ParsedQuery>();
  const [currentResult, setCurrentResult] =
    useState<null | Array<Substitution>>(null);

  useStream(props.queryResults, setCurrentResult);

  const updateQuery = (newQuery: string) => {
    setQuery(newQuery);

    if (newQuery === "") return setParsingResult(undefined);

    try {
      const result = parsePattern(newQuery);

      setParsingResult({
        _type: "success",
        result,
      });
    } catch (e) {
      const message: string = e.message;

      setCurrentResult(null);

      setParsingResult({
        _type: "failure",
        message: message.slice(
          0,
          message.indexOf(
            "Instead, I was expecting to see one of the following"
          )
        ),
      });
    }
  };

  return (
    <EditorPane
      title="Edit query"
      //disabled={props.disabled ? "Cannot query an empty project" : undefined}
    >
      <div className="query">
        <Input value={query} label="Query" setValue={updateQuery} />
        {parsingResult?._type === "failure" && (
          <div className="query__error-message">{parsingResult.message}</div>
        )}
        {currentResult && <QueryResult substitutions={currentResult} />}
        <Spacing />
        <div className="query__evaluate-button-container">
          <Button
            disabled={parsingResult?._type !== "success"}
            onClick={() => {
              if (parsingResult?._type === "success") {
                props.evaluate(parsingResult.result);
              }
            }}
          >
            Evaluate
          </Button>
        </div>
      </div>
    </EditorPane>
  );
};

export const EditorUi = (props: EditorProps) => {
  const [currentBranch, setCurrentBranch] = useState<null | [string, number]>(
    null
  );

  const [nodes, setNodes] = useImmer<NodeMemory>({});
  const [rules, setRules] = useImmer<RuleMemory>({});
  const [currentId, incrementId] = useState(0);

  const [isHidden, toggleHidden] = useBoolean(false);

  const generateId = () => {
    incrementId((old) => old + 1);
    return currentId;
  };

  const createBranch = useCallback(
    (name: string) => {
      if (!Reflect.has(rules, name)) return;

      let ruleId = generateId();

      setRules((draft: RuleMemory) => {
        draft[name].branches.push(ruleId);
      });

      props.emit({
        _type: "createBranch",
        argumentCount: nodes[name],
        name,
        branchId: ruleId,
      });

      if (currentBranch === null) {
        setCurrentBranch([name, ruleId]);
        selectBranch(name, ruleId);
      }
    },
    [rules]
  );

  const createRule = useCallback((name: string, argumentCount: number) => {
    setRules((draft: RuleMemory) => {
      draft[name] = { branches: [] };
    });

    createNode(name, argumentCount);
  }, []);

  const createNode = useCallback((name: string, argumentCount: number) => {
    setNodes((draft: NodeMemory) => {
      draft[name] = argumentCount;
    });
  }, []);

  useKey(
    "s",
    (e) => {
      if (e.target instanceof HTMLInputElement) return;
      if (currentBranch === null) return;

      toggleHidden();
      props.emit({
        _type: "togglePointerEvents",
        shouldGetEnabled: !isHidden,
      });
    },
    {
      target: window,
    },
    [currentBranch, isHidden]
  );

  const addNode = useCallback(
    (name: string) => {
      const argumentCount = nodes[name];

      props.emit({
        _type: "addNode",
        name,
        argumentCount,
      });
    },
    [nodes]
  );

  const selectBranch = useCallback((name: string, index: number) => {
    setCurrentBranch([name, index]);

    props.emit({
      _type: "editBranch",
      branchId: index,
      name,
    });
  }, []);

  const deleteBranch = useCallback((name: string, branchId: BranchId) => {
    setRules((draft: RuleMemory) => {
      const rule = draft[name].branches;

      rule.splice(rule.indexOf(branchId), 1);
    });

    props.emit({
      _type: "deleteBranch",
      branchId,
      name,
    });
  }, []);

  return (
    <div id="editor" class={isHidden ? "editor__hidden" : ""}>
      <CreateCompound
        productName={"rule"}
        isTaken={(name) => Reflect.has(nodes, name)}
        create={createRule}
      />
      <RuleList
        selectedBranch={currentBranch}
        createBranch={createBranch}
        select={selectBranch}
        delete={deleteBranch}
        edit={(name, index) => {
          selectBranch(name, index);
          toggleHidden();

          props.emit({
            _type: "togglePointerEvents",
            shouldGetEnabled: !isHidden,
          });
        }}
        disabled={Object.entries(rules).length === 0}
        rules={rules}
      />
      <CreateCompound
        disabled={currentBranch === null}
        productName="node"
        isTaken={(name) => Reflect.has(nodes, name)}
        create={createNode}
      />
      <NodeList
        use={addNode}
        disabled={currentBranch === null}
        nodes={[...Object.entries(nodes)]}
      />
      <EditQuery
        disabled={currentBranch === null}
        queryResults={props.queryResults}
        evaluate={(query) => props.emit({ _type: "evaluateQuery", query })}
      />
      <div className="editor__pane">8</div>
    </div>
  );
};
