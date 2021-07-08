const { Group } = require("@thi.ng/geom");

exports.shellGeometry = () => new Group({}, []);
exports.writeGeometry = (inner) => (geom) => () => (geom.children = [inner]);
