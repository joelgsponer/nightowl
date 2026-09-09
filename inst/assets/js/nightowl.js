/* nightowl: download the SVG next to the clicked button */
function nightowlDownloadSvg(button, filename) {
  var container = button.parentElement;
  var svg = container.querySelector("svg");
  if (!svg) return;
  var xml = new XMLSerializer().serializeToString(svg);
  var blob = new Blob([xml], { type: "image/svg+xml;charset=utf-8" });
  var url = URL.createObjectURL(blob);
  var link = document.createElement("a");
  link.href = url;
  link.download = filename || "plot.svg";
  document.body.appendChild(link);
  link.click();
  document.body.removeChild(link);
  URL.revokeObjectURL(url);
}
