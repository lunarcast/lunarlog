/** @typedef { import("@thi.ng/geom-api").IShape } Geometry */
/** @typedef { import("@thi.ng/matrices").Mat23Like } Transform */
/** @typedef { import("@thi.ng/vectors").Vec2Like } Vec2 */

const geom = require("@thi.ng/geom");
const hiccup = require("@thi.ng/hiccup-canvas");

const fromAABBLike = ({ pos, size }) => ({ position: pos, size });

exports._group = (attribs) => new geom.Group(attribs, attribs.children);
exports._rect = (attribs) =>
  new geom.Rect(attribs.position, attribs.size, attribs);
exports._circle = (attribs) =>
  new geom.Circle(attribs.position, attribs.radius, attribs);
exports._text = (attribs) =>
  new geom.Text(attribs.position, attribs.text, attribs);

exports.render = (ctx) => (geometry) => () => hiccup.draw(ctx, geometry);
exports.attributes = (geom) => geom.attribs;

exports.children = (geometry) => {
  if (geometry instanceof geom.Group) return geometry.children;

  return [];
};

exports.closestPoint = (geometry) => (point) =>
  geom.closestPoint(geometry, point, []);

// Transform related stuff
exports.transform =
  (/** @type Transform */ matrix) => (/** @type Geometry */ geometry) =>
    geom.transform(geometry, matrix);

exports.transformVertices =
  (/** @type {(vec: Vec2) => Vec2} */ f) => (/** @type Geometry */ geometry) =>
    geom.transformVertices(geometry, f);

geom.pointInside.add("group", (group, point) =>
  group.children.some((child) => geom.pointInside(child, point))
);
