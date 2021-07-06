exports.fixDpi = (/** @type CanvasRenderingContext2d */ context) => () => {
  // const dpi = Math.ceil(window.devicePixelRatio);
  const dpi = 1;

  const clientRect = context.canvas.getBoundingClientRect();

  // scale the canvas
  context.canvas.width = clientRect.width * dpi;
  context.canvas.height = clientRect.height * dpi;

  // context.setTransform(dpi, 0, 0, dpi, 0, 0);
};
