exports.windowSize = {
  read() {
    return [window.innerWidth, innerHeight];
  },
  changes(emit) {
    return () => {
      const listener = () => emit([window.innerWidth, window.innerHeight]);
      window.addEventListener("resize", listener);

      return () => window.removeEventListener("resize", listener);
    };
  },
};

exports.contextToCanvas = (ctx) => () => ctx.canvas;
