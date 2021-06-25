const vector = require("@thi.ng/vectors");

exports.distance = (a) => (b) => vector.dist2(a, b);
exports.distanceSquared = (a) => (b) => vector.distSq2(a, b);
