exports.measureText = (ctx) => (font) => (text) => {
  ctx.save();
  ctx.font = font;

  const result = ctx.measureText(text);

  ctx.restore();

  return result;
};
