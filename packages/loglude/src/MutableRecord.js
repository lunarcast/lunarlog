exports.writeImpl = (name) => (value) => (rec) => () => {
  rec[name] = value;
};

exports.readImpl = (name) => (rec) => () => rec[name];
exports.fromRecord = (record) => Object.assign({}, record);
exports.extendImpl = (name) => (value) => (previous) => {
  const newObject = Object.create(previous);
  newObject[name] = value;
  return newObject;
};
