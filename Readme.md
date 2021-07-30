# Lunarlog

Lunarlog is a simple visual logic programming language (think prolog) I developed for fun. The code is mostly written in [PureScript](https://www.purescript.org/), altho there is some [Typescript](https://www.typescriptlang.org/) used for the [preact](https://preactjs.com/) based ui.

The list with everything I have not made myself (for infoeducatie) can be found [here](./docs/external.md)

# How to run localy

First, you need to have [pnpm](https://pnpm.io/), [spago](https://github.com/purescript/spago) and purescript installed on your system.

Install dependencies by running:

```sh
pnpm install
```

Instll & build the purescript code by running

```sh
spago build
```

Bundle everything together using

```sh
node ./build.js
```

ALternatively, you can start the development server using

```sh
pnpm dev
```
