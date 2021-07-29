import "../styles/editor.scss";
import { useState, useEffect, useCallback } from "preact/hooks";
import { Button, Input } from "./Input";
import { ComponentChild } from "preact";
import { Spacing } from "./Spacing";
import { capitalize, capitalizeWord } from "./helpers";
import { ForeignAction } from "../foreign";
import { useImmer } from "use-immer";
import { useKey, useBoolean } from "react-use";
import { Icon } from "./Icon";

// ========== Types
interface RuleBranch {}

interface Rule {
  branches: Array<RuleBranch>;
}

type NodeMemory = Record<string, number>;
type RuleMemory = Record<string, Rule>;

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
  select(name: string, index: number): void;
  edit(name: string, index: number): void;
}

interface BranchListProps {
  branches: Array<RuleBranch>;
  name: string;
  selectedBranch: null | number;
  createBranch(): void;
  select(index: number): void;
  edit(index: number): void;
}

interface RuleBranchProps {
  name: string;
  isSelected: boolean;
  select(): void;
  edit(): void;
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
const RuleBranch = ({ edit, select, isSelected }: RuleBranchProps) => {
  return (
    <div
      className={["rule__branch", isSelected && "rule__branch--selected"].join(
        " "
      )}
      onClick={select}
    >
      <Icon onClick={edit}>edit</Icon>
      <Icon>delete</Icon>
    </div>
  );
};

// The rule list panel
const BranchList = ({
  branches,
  name,
  createBranch,
  selectedBranch,
  edit,
  select,
}: BranchListProps) => {
  return (
    <div className="rule">
      <div className="rule__name">{capitalizeWord(name)}</div>
      <div className="rule__branches">
        {branches.map((_, index) => (
          <RuleBranch
            isSelected={index === selectedBranch}
            name={name}
            key={index}
            select={() => select(index)}
            edit={() => edit(index)}
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
          edit={(index) => edit(name, index)}
          select={(index) => select(name, index)}
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

export const EditorUi = (props: EditorProps) => {
  const [currentBranch, setCurrentBranch] = useState<null | [string, number]>(
    null
  );

  const [nodes, setNodes] = useImmer<NodeMemory>({});
  const [rules, setRules] = useImmer<RuleMemory>({});

  const [isHidden, toggleHidden] = useBoolean(false);

  const createBranch = useCallback(
    (name: string) => {
      if (!Reflect.has(rules, name)) return;

      setRules((draft: RuleMemory) => {
        draft[name].branches.push({});
      });

      const ruleIndex = rules[name].branches.length;

      props.emit({
        _type: "createBranch",
        argumentCount: nodes[name],
        name,
        index: ruleIndex,
      });

      if (currentBranch === null) {
        setCurrentBranch([name, ruleIndex]);
        selectBranch(name, ruleIndex);
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
      index,
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
        edit={(name, index) => {
          selectBranch(name, index);
          toggleHidden();
        }}
        disabled={Object.entries(rules).length === 0}
        rules={rules}
      />
      <CreateCompound
        disabled={currentBranch === null}
        allowUpdates
        productName="node"
        isTaken={(name) => Reflect.has(nodes, name)}
        create={createNode}
      />
      <NodeList
        use={addNode}
        disabled={currentBranch === null}
        nodes={[...Object.entries(nodes)]}
      />
      <div className="editor__pane">7</div>
      <div className="editor__pane">8</div>
    </div>
  );
};
