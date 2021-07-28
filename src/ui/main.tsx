import "../styles/editor.scss";
import { useState, useEffect } from "preact/hooks";
import { Button, Input } from "./Input";
import { ComponentChild } from "preact";
import { Spacing } from "./Spacing";

// ========== Types
type NodeMemory = Record<string, number>;

interface EditorPaneProps {
  title: string;
  disabled?: string;
  children: ComponentChild;
}

interface NodeListProps {
  nodes: Array<[string, number]>;
}

interface NodeProps {
  name: string;
  argumentCount: number;
}

interface CreateNodeProps {
  isTaken(name: string): boolean;
  createNode(name: string, argumentCount: number): void;
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
  return <div className="node">{name}</div>;
};

const EmptyNodeList = () => {
  return (
    <div className="editor__pane-empty-node-list">
      <p>You haven't created any node yet!</p>
    </div>
  );
};

const NodeList = ({ nodes }: NodeListProps) => {
  return (
    <EditorPane
      disabled={"Cannot use nodes without creating a rule first"}
      title="Use node"
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

const CreateNode = ({ isTaken, createNode }: CreateNodeProps) => {
  const [name, setName] = useState("");
  const [argCount, setArgCount] = useState(2);
  let error: null | string = null;

  if (isTaken(name)) {
    error = `Name "${name}" is already taken`;
  }

  const resetInputs = () => {
    const adjective =
      nodeAdjective[Math.floor(Math.random() * nodeAdjective.length)];
    setName(`My ${adjective} node`);
    setArgCount(2);
  };

  useEffect(() => {
    resetInputs();
  }, []);

  return (
    <EditorPane title="Create node">
      <div id="create-node">
        <Input value={name} label="Node name" setValue={setName} />
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
            Create
          </Button>
        </div>
      </div>
    </EditorPane>
  );
};

export const EditorUi = () => {
  const [nodeMemory, setNodeMemory] = useState<NodeMemory>({});

  return (
    <div id="editor">
      <NodeList nodes={[...Object.entries(nodeMemory)]} />
      <CreateNode
        isTaken={(name) => Reflect.has(nodeMemory, name)}
        createNode={(name, argumentCount) =>
          setNodeMemory((old) => ({ ...old, [name]: argumentCount }))
        }
      />
      <div className="editor__pane">3</div>
      <div className="editor__pane">4</div>
      <div className="editor__pane">7</div>
      <div className="editor__pane">8</div>
    </div>
  );
};
