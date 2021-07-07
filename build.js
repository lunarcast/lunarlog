const esbuild = require("esbuild");
const PurescriptPlugin = require("esbuild-plugin-purescript");
const { sassPlugin } = require("esbuild-sass-plugin");

const production = process.env.NODE_ENV === "production";

esbuild
  .build({
    entryPoints: ["src/index.js"],
    bundle: true,
    minify: production,
    outdir: "dist",
    watch: true,
    plugins: [PurescriptPlugin(), sassPlugin()],
    sourcemap: "both",
  })
  .catch((_e) => process.exit(1));
