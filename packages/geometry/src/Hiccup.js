const geom = require("@thi.ng/geom");
const multi = require("@thi.ng/defmulti");
const _type = "purescript";

const aabbToForeign = ({ position, size }) => ({ pos: position, size });
const aabbFromForeign = ({ pos, size }) => ({ position: pos, size });

exports.buildGeometryBlueprintImpl =
  (name) =>
  ({
    toHiccup,
    bounds,
    pointInside,
    translate,
    children,
    toLocalCoordinates,
  }) => {
    const Internal = {
      [name]: class {
        constructor(attribs) {
          this.attribs = attribs;
        }

        get type() {
          return _type;
        }

        translate(amount) {
          const newAttribs = translate(amount)(this.attribs);

          return new Internal(newAttribs);
        }

        pointInside(point) {
          return pointInside(point)(this.attribs)();
        }

        bounds() {
          return aabbToForeign(bounds(this.attribs)());
        }

        copy() {
          return new Internal(this.attribs);
        }

        children() {
          return children(this.attribs)();
        }

        toLocalCoordinates(point) {
          return toLocalCoordinates(this.attribs)(point)();
        }

        toHiccup() {
          return toHiccup(this.attribs)();
        }
      },
    }[name];

    return (s) => new Internal(s);
  };

geom.translate.add(_type, (geometry, amount) => geometry.translate(amount));
geom.bounds.add(_type, (shape) => shape.bounds());
geom.pointInside.add(_type, (shape, point) => shape.pointInside(point));

exports.pointInsideGeometry = (vec) => (geometry) =>
  geom.pointInside(geometry, vec);

exports.toHiccupGeometry = (geometry) => geometry.toHiccup();
exports.boundsGeometry = (geometry) => aabbFromForeign(geom.bounds(geometry));
exports.translateGeometry = (amount) => (geometry) =>
  geom.translate(geometry, amount);

/** Get an array of all the children inside a geometry */
const childrenGeometry = multi.defmulti((x) => x.type);

childrenGeometry.add(multi.DEFAULT, () => []);
childrenGeometry.add(_type, ($) => $.children());
childrenGeometry.add("group", ($) => $.children);

exports.childrenGeometry = (geom) => childrenGeometry(geom);

/** Project a point to the coordinates inside the geometry. Only meamingful for components containing children */
const toLocalCoordinates = multi.defmulti((x) => x.type);

toLocalCoordinates.add(multi.DEFAULT, (_, vec) => vec);
toLocalCoordinates.add(_type, ($, point) => $.toLocalCoordinates(point));

exports.toLocalCoordinatesGeometry = (geometry) => (point) =>
  toLocalCoordinates(geometry, point);
