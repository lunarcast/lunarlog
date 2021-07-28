import { renderPatternToImage } from "../foreign";
import "../styles/editor.scss";
import { Icon } from "./Icon";
import { TabProps, Tabs, TabsProps } from "./Tabs";
import { useEffect, useState } from "preact/hooks";

const Nodes = () => {
  const [nodeImage, setNodeImage] = useState("");
  useEffect(() => {
    renderPatternToImage("Long name:)", 4).then(setNodeImage);
  }, []);

  return <img src={nodeImage} />;
};

const tabs: Array<TabProps> = [
  {
    name: "Nodes",
    content: <Nodes />,
  },
  { name: "Rules", content: "Hello world rules" },
  {
    name: "Query",
    content: "Heelo world queries",
  },
];

export const EditorUi = () => {
  return (
    <div id="editor">
      <Tabs tabs={tabs}></Tabs>
    </div>
  );
};
