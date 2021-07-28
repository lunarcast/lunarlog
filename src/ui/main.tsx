import "../styles/editor.scss";
import { useState, useEffect } from "preact/hooks";
import { Button, Input } from "./Input";
import { ComponentChild } from "preact";
import { Spacing } from "./Spacing";
import { capitalize, capitalizeWord } from "./helpers";
import { div } from "@thi.ng/vectors";

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
}

interface RuleBranchProps {
  thumbail: string;
  name: string;
}

interface BranchListProps {
  branches: Array<RuleBranch>;
  name: string;
}

interface CreateCompoundProps {
  isTaken(name: string): boolean;
  createNode(name: string, argumentCount: number): void;
  productName: string;
  allowUpdates?: boolean;
  disabled?: boolean;
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
const BranchList = ({ branches, name }: BranchListProps) => {
  return (
    <div className="rule">
      <div className="rule__name">{capitalizeWord(name)}</div>
      <div className="rule__branches">
        {branches.map(({ thumbail }, index) => (
          <RuleBranch name={name} thumbail={thumbail} key={index} />
        ))}
      </div>
    </div>
  );
};

const RuleList = ({ rules, disabled }: RuleListProps) => {
  return (
    <EditorPane
      title="Rules"
      disabled={disabled ? "No rules found" : undefined}
    >
      {Object.entries(rules).map(([name, branches]) => (
        <BranchList name={name} key={name} branches={branches.branches} />
      ))}
    </EditorPane>
  );
};

// Used for the create node and create rule components
const CreateCompound = ({
  isTaken,
  createNode,
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

export const EditorUi = () => {
  const [nodeMemory, setNodeMemory] = useState<NodeMemory>({});
  const [rules, setRules] = useState<RuleMemory>({
    myRule: {
      branches: [
        {
          thumbail:
            "https://images-na.ssl-images-amazon.com/images/I/41g6jROgo0L.png",
        },
        {
          thumbail:
            "https://images-na.ssl-images-amazon.com/images/I/41g6jROgo0L.png",
        },
      ],
    },
  });

  return (
    <div id="editor">
      <CreateCompound
        productName={"rule"}
        isTaken={(name) => Reflect.has(nodeMemory, name)}
        createNode={(name, argumentCount) =>
          setRules((old) => ({ ...old, [name]: { branches: [] } }))
        }
      />
      <RuleList disabled={Object.entries(rules).length === 0} rules={rules} />
      <CreateCompound
        disabled
        allowUpdates
        productName="node"
        isTaken={(name) => Reflect.has(nodeMemory, name)}
        createNode={(name, argumentCount) =>
          setNodeMemory((old) => ({ ...old, [name]: argumentCount }))
        }
      />
      <NodeList disabled nodes={[...Object.entries(nodeMemory)]} />
      <div className="editor__pane">7</div>
      <div className="editor__pane">8</div>
    </div>
  );
};
