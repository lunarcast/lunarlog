import { render } from "preact";
import { ForeignAction, ForeignThumbail, InitialState, main } from "./foreign";
import { create, streamFromForeign } from "./Stream";
import "./styles/index.scss";

import { EditorUi } from "./ui/main";

const editorUi = document.getElementById("app__ui");

if (editorUi !== null) {
  const [actionStream, emitAction] = create<ForeignAction>();
  const [thumbails, emitThumbail] = create<ForeignThumbail>();

  const init = (name: string, argumentCount: number) => {
    const [{ thumbails }, _cancelPurescript] = main(
      { argumentCount, name },
      actionStream
    );

    streamFromForeign(thumbails)(emitThumbail);
  };

  render(
    <EditorUi initializeEditor={init} emit={emitAction} thumails={thumbails} />,
    editorUi
  );
}
