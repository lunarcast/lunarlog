const esbuild = require("esbuild");
const PurescriptPlugin = require("esbuild-plugin-purescript");
const { sassPlugin } = require("esbuild-sass-plugin");

const production = process.env.NODE_ENV === "production";

esbuild
  .build({
    entryPoints: ["src/index.tsx"],
    bundle: true,
    minify: production,
    outdir: "dist",
    watch: true,
    plugins: [PurescriptPlugin(), sassPlugin()],
    sourcemap: "both",
    jsxFactory: "h",
    jsxFragment: "Fragment",
    inject: ["./src/preact-shim.ts"],
    loader: {
      ".svg": "file",
    },
  })
  .catch((_e) => process.exit(1));
