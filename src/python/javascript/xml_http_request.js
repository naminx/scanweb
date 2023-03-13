const url = arguments[0];
const resolve = arguments[arguments.length - 1];
const magic = "xmlHttpRequest";
const input = document.createElement("input");
const uid = () => {
  const tmp = performance.now().toString(36).replace(/\./g, "");
  return document.getElementById(tmp) == undefined ? tmp : uid();
};
input.id = uid();
input.type = "hidden";
document.querySelectorAll("body")[0].append(input);
const handler = () => {
  input.removeEventListener(input.id, handler);
  const result = input.value;
  delete input.id;
  input.remove();
  resolve(result);
};
input.addEventListener(input.id, handler);
const location = window.location;
const origin = location.protocol + "//" + location.hostname + "/";
window.postMessage(
  JSON.stringify({
    magic: magic,
    url: url,
    id: input.id,
  }),
  origin
);
