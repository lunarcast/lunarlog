const { add2 } = require("@thi.ng/vectors");

exports.buildGeometryBlueprint = ({ _type, toHiccup, aabbLike }) => {
  class Internal {
    constructor(attribs) {
      this.attribs = attribs;

      if (aabbLike) {
        const aabb = aabbLike(attribs)();
        this.pos = aabb.position;
        this.size = aabb.size;
        this.max = () => add2([], this.pos, this.size);
      }
    }

    get type() {
      return _type;
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
