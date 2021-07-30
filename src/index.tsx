import { render } from "preact";
import { ForeignAction, main } from "./foreign";
import { create, streamFromForeign, useStream } from "./Stream";
import { useState } from "preact/hooks";
import "./styles/index.scss";

import { EditorUi, languageContext, languages } from "./ui/main";

const editorUi = document.getElementById("app__ui");

if (editorUi !== null) {
  const [actionStream, emitAction] = create<ForeignAction>();
  const [result] = main(actionStream);
  const [currentLanguageStream, emitLanguage] = create<number>();

  const Root = () => {
    const [currentLanguage, setCurrentLanguage] = useState(0);

    useStream(currentLanguageStream, setCurrentLanguage);

    return (
      <languageContext.Provider value={currentLanguage}>
        <EditorUi
          nextLanguage={() =>
            emitLanguage((currentLanguage + 1) % languages.length)
          }
          queryResults={streamFromForeign(result.queryResults)}
          emit={emitAction}
        />
        ,
      </languageContext.Provider>
    );
  };

  render(<Root />, editorUi);
}
