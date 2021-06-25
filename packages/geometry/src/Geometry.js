const geom = require("@thi.ng/geom");
const hiccup = require("@thi.ng/hiccup-canvas");

exports._group = () => (attribs) => (children) =>
  new geom.Group(attribs, children);
exports._rect = () => (attribs) => (pos) => (size) =>
  new geom.Rect(pos, size, attribs);
exports._circle = () => (attribs) => (pos) => (radius) =>
  new geom.Circle(pos, radius, attribs);
exports._ref = () => (attribs) => (inner) => new geom.Group(attribs, [inner]);

exports.fromRef = (i) => i;
exports.write = (on) => (inner) => () => {
  on.children[0] = inner;
};

exports.render = (ctx) => (geometry) => () => hiccup.draw(ctx, geometry);
