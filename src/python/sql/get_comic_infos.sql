SELECT urls.comic, subtable.url, comics.title, comics.folder,
       comics.volume, comics.chapter, subtable.idx
  FROM ((webs INNER JOIN urls
    ON webs.web = urls.web) INNER JOIN comics
    ON urls.comic = comics.comic) INNER JOIN (
       SELECT *
         FROM (SELECT 0 AS idx, '' AS url
               UNION ALL
               VALUES {values})
        LIMIT -1 OFFSET 1)
    AS subtable
 WHERE 'https://' || webs.domain || urls.path = subtable.url
 ORDER BY subtable.idx;
