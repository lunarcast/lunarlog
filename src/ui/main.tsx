import "../styles/editor.scss";
import { useState, useEffect, useCallback } from "preact/hooks";
import { Button, Input } from "./Input";
import { ComponentChild } from "preact";
import { Spacing } from "./Spacing";
import { capitalize, capitalizeWord } from "./helpers";
import { ForeignAction, ForeignThumbail } from "../foreign";
import { Stream } from "../Stream";
import { useImmer } from "use-immer";
import { useKey, useBoolean } from "react-use";
import { Icon } from "./Icon";

// ========== Types
interface RuleBranch {
  thumbail: string;
}

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
}

interface NodeProps {
  name: string;
  argumentCount: number;
}

interface RuleListProps {
  rules: RuleMemory;
  disabled?: boolean;
  createBranch(name: string): void;
}

interface RuleBranchProps {
  thumbail: string;
  name: string;
}

interface BranchListProps {
  branches: Array<RuleBranch>;
  name: string;
  createBranch(): void;
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
  thumails: Stream<ForeignThumbail>;
  initializeEditor(name: string, argumentCount: number): void;
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

const Node = ({ argumentCount, name }: NodeProps) => {
  return (
    <div className="node">
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
const NodeList = ({ nodes, disabled }: NodeListProps) => {
  return (
    <EditorPane
      title="Use node"
      disabled={
        disabled ? "Cannot use nodes without creating a rule first" : undefined
      }
    >
      <div id="node-list">
        {nodes.map(([name, argumentCount]) => (
          <Node name={name} key={name} argumentCount={argumentCount} />
        ))}
      </div>
      {nodes.length === 0 && <EmptyNodeList />}
    </EditorPane>
  );
};

// The preview for a rule
const RuleBranch = ({ thumbail, name }: RuleBranchProps) => {
  return (
    <div className="rule__branch">
      <img
        src={thumbail}
        height={100}
        alt={name}
        className="rule__branch-thumbail"
      />
    </div>
  );
};

// The rule list panel
const BranchList = ({ branches, name, createBranch }: BranchListProps) => {
  return (
    <div className="rule">
      <div className="rule__name">{capitalizeWord(name)}</div>
      <div className="rule__branches">
        <button className="rule__create-branch" onClick={createBranch}>
          <Icon>add</Icon>
        </button>
        {branches.map(({ thumbail }, index) => (
          <RuleBranch name={name} thumbail={thumbail} key={index} />
        ))}
      </div>
    </div>
  );
};

const RuleList = ({ rules, disabled, createBranch }: RuleListProps) => {
  return (
    <EditorPane
      title="Rules"
      disabled={disabled ? "No rules found" : undefined}
    >
      {Object.entries(rules).map(([name, branches]) => (
        <BranchList
          createBranch={() => createBranch(name)}
          name={name}
          key={name}
          branches={branches.branches}
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
  const [initializedPurescript, setInitializedPurescript] = useState(false);
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
        draft[name].branches.push({
          thumbail: "",
        });
      });

      if (!initializedPurescript) {
        props.initializeEditor(name, nodes[name]);
        setInitializedPurescript(true);
      }
    },
    [rules]
  );

  const createRule = useCallback((name: string, argumentCount: number) => {
    setRules((draft: RuleMemory) => {
      draft[name] = { branches: [] };
    });

    createNode(name, argumentCount);

    props.emit({
      _type: "createRule",
      name,
    });
  }, []);

  const createNode = useCallback((name: string, argumentCount: number) => {
    setNodes((draft: NodeMemory) => {
      draft[name] = argumentCount;
    });
  }, []);

  const toggleEditor = useCallback(() => {
    toggleHidden();
  }, []);

  useKey("s", toggleEditor, {
    target: window,
  });

  return (
    <div id="editor" class={isHidden ? "editor__hidden" : ""}>
      <CreateCompound
        productName={"rule"}
        isTaken={(name) => Reflect.has(nodes, name)}
        create={createRule}
      />
      <RuleList
        createBranch={createBranch}
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
        disabled={currentBranch === null}
        nodes={[...Object.entries(nodes)]}
      />
      <div className="editor__pane">7</div>
      <div className="editor__pane">8</div>
    </div>
  );
};
