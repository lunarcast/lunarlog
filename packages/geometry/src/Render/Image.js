exports.createOffscreenCanvas = (size) => () => {
  return new OffscreenCanvas(size[0], size[1]);
};

exports.toImageData = (canvas) => () => {
  const converter = canvas.convertToBlob
    ? "convertToBlob" // specs
    : "toBlob"; // firefox

  return canvas[converter]().then(
    (blob) =>
      new Promise((resolve) => {
        const dataUrl = new FileReader();

        dataUrl.onloadend = () => resolve(dataUrl.result);
        dataUrl.readAsDataURL(blob);
      })
  );
};
