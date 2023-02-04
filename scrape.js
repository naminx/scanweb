==============================================================================
common: minified
==============================================================================
var d=document;var xpaths=p=>(r=>Array.from({length:r.snapshotLength},(_,i)=>r.snapshotItem(i)))(d.evaluate(p,d,null,7));var xpath=(p,r=d)=>d.evaluate(p,r,null,9).singleNodeValue;

==============================================================================
common: readable
==============================================================================
const xpaths = (path) => {
  var result = document.evaluate(path, document, null, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE);
  return Array.from({ length: result.snapshotLength }, (_, index) => result.snapshotItem(index));
};
const xpath = (path, root = document) =>
  document.evaluate(path, root, null, XPathResult.FIRST_ORDERED_NODE_TYPE).singleNodeValue;

==============================================================================
mangaraw.so
==============================================================================
return xpaths('//div[@class="container"]/div[contains(@class, "section-container")]//div[contains(@class, "manga-item")]/a[2]/parent::div').map((div) => ({
  url: xpath('a[2]', div).href,
  chapter: xpath('a[3]', div).textContent.replace(/.*【第(.*)話】.*/, '$1')
}));

return xpath('//div[contains(@class, "chapters-container")]//a').href;

return xpaths('//div[contains(@class, "chapters-container")]//a').reverse().map((a) => ({
  chapter: a.title.replace(/.*【第(.*)話】.*/, '$1'),
  url: a.href
}));

return xpaths('//div[contains(@class, "card-wrap")]/img').map((img) => img.dataset.src);

==============================================================================
mangaraw.io:
==============================================================================
return xpaths('//div[contains(@class, "card-body")]/div[contains(@class, "rotate-img")]/a').map((a) => ({
  url: a.href
}));

return xpath('//div[@class="list-scoll"]//a').href;

return xpaths('//div[@class="list-scoll"]//a').reverse().map((a) => ({
  chapter: a.title.replace(/.*【第(.*)話】.*/, '$1'),
  url: a.href
}));

return xpaths('//div[contains(@class, "card-wrap")]/img').map((img) => img.dataset.src);

==============================================================================
manga1001.su:
==============================================================================
return xpaths('//h3[@class="entry-title"]/a').map((a) => ({ url: a.href }));

return xpath('//div[@class="chapterlist"]//a').href;

return xpaths('//div[@class="chapterlist"]//a').reverse().map((a) => ({
  chapter: a.title.replace(/.*【第(.*)話】.*/, '$1'),
  url: a.href
}));

return xpaths('//div[@class="entry-content"]//img').map((img) => img.dataset.src);

}))

==============================================================================
weloma.net:
==============================================================================
return xpaths(
  '//div[@id="history"]/div/div[contains(@class, "thumb-wrapper")]'
).map((div) => ({
  url: xpath('a', div).href,
  chapter: xpath('a//following-sibling::div/div[contains(@class, "chapter-title")]', div).title.replace(/^Chap /, "")
}));

return xpath('//small/div[3]/a').href;

return xpaths('//ul[contains(@class, "list-chapters")]/a').reverse().map((a) => ({
  chapter: a.title.replace(/^Chapter /, ""),
  url: a.href
}));

return xpaths('//div[contains(@class, "chapter-content")]/img').map((img) =>
  img.dataset.src.replace(/\s/g, "")
);

==============================================================================
welovemanga.one:
==============================================================================
return xpaths(
  '//div[@id="history"]/div/div[contains(@class, "thumb-wrapper")]'
).map((div) => ({
  url: xpath('following-sibling::div[contains(@class, "series-title")]/a', div).href,
  chapter: xpath('a//following-sibling::div/div[contains(@class, "chapter-title")]', div).title.replace(/^Chap /, "")
}));

==============================================================================
mangagun.com
==============================================================================
return xpaths('//div[contains(@class, "thumb-item-flow")]').map((div) => ({
  url: xpath('div[contains(@class, "series-title")]/a', div).href,
  chapter: xpath('div/div/div[contains(@class, "chapter-title")]/a', div).textContent
}));

return xpath('//div[@id="bt-reading"]/a').href;

return xpaths(
  '//ul[contains(concat(" ", normalize-space(@class), " "), " list-chapters ")]/a'
).reverse().map((a) => ({
  chapter: a.title.replace(/^Chapter /, ''),
  url: a.href
}));

return xpaths('//div[@class="chapter-content"]//img').map((img) => img.dataset.src);

==============================================================================
klmanga.net:
==============================================================================
return xpaths(
  '//div[contains(@class, "bodythumb")]//div[contains(@class, "thumb-wrapper")]'
).map((div) => ({
  url: xpath('a', div).href,
  chapter: xpath('a//following-sibling::div/div[contains(@class, "chapter-title")]', div).textContent.trim().replace(/^Last chapter: /, "")
}));

return xpaths('//div[@id="tab-chapper")//a').reverse().map((a) => ({
  chapter: a.title.replace(/^Chapter /, ""),
  url: a.href,
}));

==============================================================================
hachimanga.com
==============================================================================
return xpaths(
  '//div[contains(@class, "item-summary")]'
).map((div) => {
  const url = xpath('div[contains(@class, "post-title")]/h3/a', div).href;
  const release = xpath('div[contains(@class, "list-chapter")]/div[contains(@class, "chapter-item")]//a', div).textContent;
  if (release.match(/.*第(.*)巻.*/)) {
    return { url: url, volume: release.replace(/.*第(.*)巻.*/, '$1') };
  } else
    return { url: url, chapter: release.replace(/.*第(.*)話.*/, '$1') };
  }
});

return xpath('//div[contains(@class, "listing-chapters_wrap")]/ul/li/a').href;

return xpaths(
  '//div[contains(@class, "listing-chapters_wrap")]/ul/li/a'
).reverse().map((a) => {
  const release = a.textContent;
  if (release.match(/.*第(.*)巻.*/)) {
    return { volume: release.replace(/.*第(.*)巻.*/, '$1').trim()
           , url: a.href };
  } else {
    return { chapter: release.replace(/.*第(.*)話.*/, '$1').trim()
           , url: a.href };
  }
});

return xpaths('//div[contains(@class, "read-container")]/div[contains(@class, "reading-content")]//img').map((img) => img.dataset.src.trim());

==============================================================================
j8jp.com
==============================================================================
return xpaths('//div[@class="luf"]').map((div) => ({
  url: xpath('a', div).href,
  chapter: xpath('ul/li/a', div).textContent.replace(/Ch\. /, '')
}));

return xpath('//div[2][@class="inepcx"]/a').href;

return xpaths('//ul[@class="clstyle"]/li').reverse().map((li) => ({
  chapter: li.dataset.num,
  url: xpath('div/div/a', li).href
}));

return xpaths('//div[@id="readerarea"]/img').map((img) => img.src);

==============================================================================
mangago.me
==============================================================================
return [];

return xpath('//table[@id="chapter_table"]//a[@class="chico"]').href;

return xpaths('//table[@id="chapter_table"]//a[@class="chico"]').reverse().map({
  chapter: a.textContent.trim().replace(/Ch\./,''),
  url: a.href
});

