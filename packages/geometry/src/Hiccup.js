const { add2 } = require("@thi.ng/vectors");
const geom = require("@thi.ng/geom");

const _type = "purescript";

exports.buildGeometryBlueprint = ({ toHiccup, aabbLike, translate }) => {
  class Internal {
    constructor(attribs) {
      this.attribs = attribs;

      if (aabbLike) {
        const aabb = aabbLike(attribs)();

        this.bounds = { pos: aabb.position, size: aabb.size };
        this.aabb = new geom.Rect(aabb.position, aabb.size);
      }
    }

    get type() {
      return _type;
    }

    translate(amount) {
      const newAttribs = translate(amount)(this.attribs);

      return new Internal(newAttribs);
    }

    pointInside(point) {
      if (aabbLike) return geom.pointInside(this.aabb, point);

      return false;
    }

    copy() {
      return new Internal(this.attribs);
    }

    toHiccup() {
      return toHiccup(this.attribs);
    }
  }

  return (s) => new Internal(s);
};

geom.translate.add(_type, (geometry, amount) => geometry.translate(amount));
geom.bounds.add(_type, (shape) => shape.bounds);
geom.pointInside.add(_type, (shape, point) => shape.pointInside(point));
