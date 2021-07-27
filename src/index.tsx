import { render } from "preact";
import "./styles/index.scss";

// @ts-ignore purescript code
import { main } from "../output/Main";
import { EditorUi } from "./ui/main";

const editorUi = document.getElementById("app__ui");

if (editorUi !== null) {
  main();
  render(<EditorUi />, editorUi);
}
