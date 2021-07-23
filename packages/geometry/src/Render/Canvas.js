/**
 * Taken from https://github.com/thi-ng/umbrella/blob/develop/packages/hiccup-canvas/src/state.ts
 */
const CTX_ATTRIBS = {
  align: "textAlign",
  alpha: "globalAlpha",
  baseline: "textBaseline",
  clip: "clip",
  compose: "globalCompositeOperation",
  dash: "!setLineDash",
  dashOffset: "lineDashOffset",
  direction: "direction",
  fill: "fillStyle",
  filter: "filter",
  font: "font",
  lineCap: "lineCap",
  lineJoin: "lineJoin",
  miterLimit: "miterLimit",
  shadowBlur: "shadowBlur",
  shadowColor: "shadowColor",
  shadowX: "shadowOffsetX",
  shadowY: "shadowOffsetY",
  smooth: "imageSmoothingEnabled",
  stroke: "strokeStyle",
  weight: "lineWidth",
};

/**
 * Apply a record with rendering settings to a rendering conrtext.
 *
 * @param {CanvasRenderingContext2d} ctx The context to mutate.
 */
exports.setAttributes = (ctx) => (attributes) => () => {
  for (const key in attributes) {
    if (!CTX_ATTRIBS[key]) continue;
    if (CTX_ATTRIBS[key][0] === "!") {
      ctx[CTX_ATTRIBS[key].slice(1)](attributes[key]);
      continue;
    }

    ctx[CTX_ATTRIBS[key]] = attributes[key];
  }
};

exports.transform = (ctx) => (matrix) => () => ctx.transform(...matrix);
