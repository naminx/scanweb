const xpaths = (path) => {
  const query = document.evaluate(path, document, null, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);
  return Array(query.snapshotLength).fill(0).map((_, index) => query.snapshotItem(index));
};
const xpath = (path, root) =>
  document.evaluate(path, root, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue;
return xpaths(
  '//div[contains(concat(" ",normalize-space(@class)," ")," data ")][contains(concat(" ",normalize-space(@class)," ")," wleft ")]'
).filter((div) => xpath('div[contains(concat(" ",normalize-space(@class)," ")," list-chapter ")][contains(concat(" ",normalize-space(@class)," ")," wleft ")]//span[@class="chapter"]/a', div).length > 0).map((div) => {
  const url = xpath("h3/a", div).href;
  const release = xpath('div[contains(concat(" ",normalize-space(@class)," ")," list-chapter ")][contains(concat(" ",normalize-space(@class)," ")," wleft ")]//span[@class="chapter"]/a', div).textContent;
  if (release.match(/.*第(.*)巻.*/)) {
      return { url: url, volume: release.replace(/.*第(.*)巻.*/, "$1") };
  } else {
      return { url: url, chapter: release.replace(/.*第(.*)話.*/, "$1") };
  }
});
