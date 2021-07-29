import { render } from "preact";
import { ForeignAction, main } from "./foreign";
import { create } from "./Stream";
import "./styles/index.scss";

import { EditorUi } from "./ui/main";

const editorUi = document.getElementById("app__ui");

if (editorUi !== null) {
  const [actionStream, emitAction] = create<ForeignAction>();

  const init = (
    path: [string, number],
    name: string,
    argumentCount: number
  ) => {
    const _ = main(path, { argumentCount, name }, actionStream);
  };

  render(<EditorUi initializeEditor={init} emit={emitAction} />, editorUi);
}
