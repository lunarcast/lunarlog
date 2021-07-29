import { render } from "preact";
import { ForeignAction, main } from "./foreign";
import { create, streamFromForeign } from "./Stream";
import "./styles/index.scss";

import { EditorUi } from "./ui/main";

const editorUi = document.getElementById("app__ui");

if (editorUi !== null) {
  const [actionStream, emitAction] = create<ForeignAction>();

  const [result] = main(actionStream);

  render(
    <EditorUi
      queryResults={streamFromForeign(result.queryResults)}
      emit={emitAction}
    />,
    editorUi
  );
}
