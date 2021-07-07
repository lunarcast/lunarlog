const { add2 } = require("@thi.ng/vectors");
const geom = require("@thi.ng/geom");

const _type = "purescript";

const aabbToForeign = ({ position, size }) => ({ pos: position, size });
const aabbFromForeign = ({ pos, size }) => ({ position: pos, size });

exports.buildGeometryBlueprintImpl =
  (name) =>
  ({ toHiccup, bounds, pointInside, translate }) => {
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
          return pointInside(point)(this.attribs);
        }

        bounds() {
          return aabbToForeign(bounds(this.attribs));
        }

        copy() {
          return new Internal(this.attribs);
        }

        toHiccup() {
          return toHiccup(this.attribs);
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
