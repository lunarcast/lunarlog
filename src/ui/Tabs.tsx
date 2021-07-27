import { useState } from "preact/hooks";
import { ComponentChild } from "preact";

export interface TabProps {
  name: string;
  content: ComponentChild;
}

export interface TabsProps {
  tabs: Array<TabProps>;
  headerStart?: ComponentChild;
  headerEnd?: ComponentChild;
}

const nullableToArray = <T,>(v: T | null | undefined): T[] =>
  v === null || v === undefined ? [] : [v];

export const Tabs = ({ tabs, headerStart, headerEnd }: TabsProps) => {
  const [currentTab, setCurrentTab] = useState(tabs[0].name);

  const tabHtml = (props: TabProps) => (
    <button
      key={props.name}
      onClick={() => setCurrentTab(props.name)}
      className={
        "tabs__tab" + (props.name === currentTab ? " tabs__tab--current" : "")
      }
    >
      {props.name}
    </button>
  );

  const header = [
    ...nullableToArray(headerStart),
    ...tabs.map(tabHtml),
    ...nullableToArray(headerEnd),
  ];

  const content = tabs.find((tab) => tab.name === currentTab);

  if (content === undefined) throw new Error(`Cannot find tab ${currentTab}`);

  return (
    <div className="tabs">
      <header className="tabs__header">{header}</header>
      <main className="tabs__content">{content.content}</main>
    </div>
  );
};
