import "../styles/editor.scss";
import { Icon } from "./Icon";
import { TabProps, Tabs, TabsProps } from "./Tabs";

interface NavigationLinkProps {
  children: string;
  activeTab: string;
  icon: string;
}

export const NavigationLink = ({
  children: name,
  activeTab,
  icon,
}: NavigationLinkProps) => {
  return (
    <div className="editor__navigation-link-container">
      <div className="editor__navigation-link-icon">
        <Icon>{icon}</Icon>
      </div>
      <div className="editor__navigation-link">{name}</div>
    </div>
  );
};

const tabs: Array<TabProps> = [
  { name: "Nodes", content: "Hello world" },
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
