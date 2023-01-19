weloma.net:
const xpaths = (path) => {
  const query = document.evaluate(path, document, null, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);
  return Array(query.snapshotLength).fill(0).map((_, index) => query.snapshotItem(index));
};
const xpath = (path, root = document) =>
  document.evaluate(path, root, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue;

return xpaths(
  '//div[@id="history"]/div/div[contains(@class, "thumb-wrapper")]'
).map((div) => ({
  url: xpath('a', div).href,
  chapter: xpath('a//following-sibling::div/div[contains(@class, "chapter-title")]', div).title.replace(/^Chap /, "")
}));

return xpath('//small/div[3]/a').href;

return xpaths('//ul[contains(@class, "list-chapters")]/a').reverse().map((a) => ({
  url: a.href,
  chapter: a.title.replace(/^Chapter /, ""),
}));

return xpaths('//div[contains(@class, "chapter-content")]/img').map((a) =>
  a.dataset.src.replace(/\s/g, "")
);
==============================================================================
welovemanga.one:
return xpaths(
  '//div[@id="history"]/div/div[contains(@class, "thumb-wrapper")]'
).map((div) => ({
  url: xpath('following-sibling::div[contains(@class, "series-title")]/a', div).href,
  chapter: xpath('a//following-sibling::div/div[contains(@class, "chapter-title")]', div).title.replace(/^Chap /, "")
}));
==============================================================================
klmanga.net:
return xpaths(
  '//div[contains(@class, "bodythumb")]//div[contains(@class, "thumb-wrapper")]'
).map((div) => ({
  url: xpath('a', div).href,
  chapter: xpath('a//following-sibling::div/div[contains(@class, "chapter-title")]', div).textContent.trim().replace(/^Last chapter: /, "")
}));

return xpaths('//div[@id="tab-chapper")//a').reverse().map((a) => ({
  url: a.href,
  chapter: a.title.replace(/^Chapter /, ""),
}));

