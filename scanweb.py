#!/usr/bin/env python3

from selenium import webdriver
from selenium.webdriver.chrome.webdriver import WebDriver
from sqlite3 import Connection
from typing import List
import sqlite3

# timeout in milliseconds
max_timeout = 1000 * 30


class ReleaseInfo:
    def __init__(self, record: dict):
        if record["chapter"] is not None:
            self.chapter = record["chapter"]
        elif record["volume"] is not None:
            self.volume = record["volume"]
        self.url = record["url"]


class WebInfo:
    def __init__(self, record: tuple):
        _web = 0
        _domain = 1
        _user_name = 2
        _password = 3
        _sentinel = 4
        _gen_url = 5
        _is_loaded = 6
        _scrape_comics = 7
        _scrape_latest = 8
        _scrape_chapters = 9
        _scrape_images = 10
        self.web = record[_web]
        self.domain = record[_domain]
        self.user_name = record[_user_name]
        self.password = record[_password]
        self.sentinel = record[_sentinel]
        self.gen_url = record[_gen_url]
        self.is_loaded = record[_is_loaded]
        self.scrape_comics = record[_scrape_comics]
        self.scrape_latest = record[_scrape_latest]
        self.scrape_chapters = record[_scrape_chapters]
        self.scrape_images = record[_scrape_images]


class ComicInfo:
    def __init__(self, record: tuple, new_release: ReleaseInfo = None):
        _comic = 0
        _url = 1
        _title = 2
        _folder = 3
        _volume = 4
        _chapter = 5
        _idx = 6
        self.comic = record[_comic]
        self.url = record[_url]
        self.title = record[_title]
        self.folder = record[_folder]
        self.volume = record[_volume]
        self.chapter = record[_chapter]
        self.idx = record[_idx]
        self.new_release = new_release


def get_webs_table(con: Connection) -> dict[int, WebInfo]:
    query = """
        SELECT
            *
        FROM
            webs;
  """
    cur = con.cursor()
    res = cur.execute(query)
    return dict(map(lambda row: (row[0], WebInfo(row)), res.fetchall()))


def query_comics(con: Connection, relinfos: list[ReleaseInfo]) -> map:
    full_path = "'https://' || webs.domain || urls.path"
    query = """
        SELECT urls.comic, {full_path}, comics.title, comics.folder,
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
            ON {full_path} = subtable.url
        ORDER BY
            subtable.idx;
  """.format(
        full_path=full_path,
        values=",".join(["({i},?)".format(i=i) for i,_ in enumerate(relinfos)])
    )
    cur = con.cursor()
    res = cur.execute(query, list(map(lambda x: x.url, relinfos)))
    return map(lambda row: ComicInfo(row), res.fetchall())


def wait_until(chrome: WebDriver, pred: str, timeout: int = max_timeout) -> None:
    js_wait_until = """
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
            }, 200);
        });
  """
    chrome.execute_script(js_wait_until, pred, timeout)


new_rels = [
    {"chapter": "119", "url": "https://weloma.art/860/"},
    {"chapter": "32", "url": "https://weloma.art/2559/"},
    {"chapter": "24", "url": "https://weloma.art/3055/"},
    {"chapter": "29.2", "url": "https://weloma.art/283/"},
    {"chapter": "19.1", "url": "https://weloma.art/2404/"},
    {"chapter": "14.1", "url": "https://weloma.art/2532/"},
    {"chapter": "23", "url": "https://weloma.art/2608/"},
    {"chapter": "17", "url": "https://weloma.art/3141/"},
    {"chapter": "47.3", "url": "https://weloma.art/142/"},
    {"chapter": "225", "url": "https://weloma.art/2317/"},
    {"chapter": "9", "url": "https://weloma.art/2990/"},
    {"chapter": "2", "url": "https://weloma.art/3435/"},
    {"chapter": "1", "url": "https://weloma.art/3438/"},
    {"chapter": "20", "url": "https://weloma.art/1706/"},
    {"chapter": "16", "url": "https://weloma.art/2035/"},
    {"chapter": "28", "url": "https://weloma.art/3347/"},
    {"chapter": "13", "url": "https://weloma.art/3400/"},
    {"chapter": "6", "url": "https://weloma.art/3421/"},
    {"chapter": "5.1", "url": "https://weloma.art/3424/"},
    {"chapter": "35", "url": "https://weloma.art/2542/"},
]

subtable = """
    SELECT
      *
    FROM
      (
        SELECT
          0 AS volume,
          '' AS chapter,
          '' AS url
        UNION ALL
        VALUES
          {values}
      )
      """

configs = ["wsl", "replit"]
config = configs[0]

chrome_options = webdriver.ChromeOptions()
match config:
    case "wsl":
        chrome_options.add_argument("--user-data-dir=/home/namin/.config/google-chrome")
        chrome_options.binary_location = "/nix/store/4bp8w2xxx2b52dq43n1kwbhny8ja46w6-google-chrome-110.0.5481.77/bin/google-chrome-stable"
    case "replit":
        chrome_options.add_argument("--no-sandbox")
        chrome_options.add_argument(
            "--user-data-dir=/home/runner/pyscan/.config/chromium"
        )
        chrome_options.binary_location = "/nix/store/x205pbkd5xh5g4iv0g58xjla55has3cx-chromium-108.0.5359.94/bin/chromium"


chrome = webdriver.Chrome(options=chrome_options)
list_of_webs = [3]
max_page = 5


con = sqlite3.connect("scanweb.sqlite3")
rows = get_webs_table(con)
for web in list_of_webs:
    for page in [*range(1, max_page + 1)]:
        webinfo = rows[web]
        result = chrome.execute_script(webinfo.gen_url, page)
        chrome.get("https://" + webinfo.domain + result)
        wait_until(chrome, webinfo.is_loaded)
        res = chrome.execute_script(webinfo.scrape_comics)
        urls = list(map(lambda row: ReleaseInfo(row), res))
        for comic in query_comics(con, urls):
            print(
                comic.title
                + " (Vol."
                + str(comic.volume)
                + ", Ch."
                + comic.chapter
                + ")"
            )
            chrome.get(comic.url)
