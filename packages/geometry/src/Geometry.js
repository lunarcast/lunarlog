const geom = require("@thi.ng/geom");
const hiccup = require("@thi.ng/hiccup-canvas");

exports._group = (attribs) => new geom.Group(attribs, attribs.children);
exports._rect = (attribs) =>
  new geom.Rect(attribs.position, attribs.size, attribs);
exports._circle = (attribs) =>
  new geom.Circle(attribs.position, attribs.radius, attribs);

exports.render = (ctx) => (geometry) => () => hiccup.draw(ctx, geometry);
exports.attributes = (geom) => geom.attribs;

exports.children = (geometry) => {
  if (geometry instanceof geom.Group) return geometry.children;

  return [];
};

exports.pointInside = (geometry) => (point) =>
  geom.pointInside(geometry, point);

exports.closestPoint = (geometry) => (point) =>
  geom.closestPoint(geometry, point, []);
