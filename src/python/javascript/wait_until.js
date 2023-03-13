return await new Promise((resolve, reject) => {
  const timeWas = new Date();
  const pred = new Function("return (" + arguments[0] + ");");
  const timeout = arguments[1];
  const wait = setInterval(function () {
    const timespan = new Date() - timeWas;
    if (pred() || timespan > timeout) {
      clearInterval(wait);
      resolve();
    }
  }, 1000);
});
