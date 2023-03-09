#!/usr/bin/env python3

from colorama import init as colorama_init
from colorama import Fore
from colorama import Style
from itertools import takewhile
from os import mkdir
from pathlib import Path
from selenium import webdriver
from selenium.webdriver.chrome.webdriver import WebDriver
from sqlite3 import Connection
from typing import Callable
from typing import List
from w3lib.url import parse_data_uri
import sqlite3

# set config = configs[0] if running in wsl
# set config = configs[1] if running in repl.it
configs: list[str] = ["wsl", "replit"]
config: str = configs[1]


blue: str = f"{Fore.BLUE}"
gray: str = f"{Fore.BLACK}{Style.BRIGHT}"
green: str = f"{Fore.GREEN}"
red: str = f"{Fore.RED}"
yellow: str = f"{Fore.YELLOW}"
reset: str = f"{Style.RESET_ALL}"


class ReleaseInfo:
    def __init__(self, record: dict):
        # chapter has priority over volume
        if "chapter" in record:
            self.chapter = record["chapter"]
        elif "volume" in record:
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
        title = record[_title]
        volume = record[_volume]
        chapter = record[_chapter]
        self.info = f"{yellow}{title} {gray}(Vol.{volume}, Ch.{chapter}){reset}"


def get_web_infos(con: Connection) -> dict[int, WebInfo]:
    query = """ SELECT * FROM webs; """
    cur = con.cursor()
    result = cur.execute(query)
    return dict(map(lambda row: (row[0], WebInfo(row)), result.fetchall()))


def query_comics(con: Connection, releases: list[ReleaseInfo]) -> map:
    values = ",".join([f"({i},?)" for i, _ in enumerate(releases)])
    query = f"""
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
  """
    # con.set_trace_callback(print)
    cur = con.cursor()
    result = cur.execute(query, [*map(lambda release: release.url, releases)])
    return map(lambda row: ComicInfo(row), result.fetchall())


def wait_until(chrome: WebDriver, pred: str, timeout: int = -1) -> None:
    if timeout == -1:
        timeout = max_timeout
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
            }, 1000);
        });
  """
    chrome.execute_script(js_wait_until, pred, timeout)


chrome_options = webdriver.ChromeOptions()
match config:
    case "wsl":
        chrome_options.add_argument("--user-data-dir=/home/namin/.config/google-chrome")
        chrome_options.binary_location = "/nix/store/4bp8w2xxx2b52dq43n1kwbhny8ja46w6-google-chrome-110.0.5481.77/bin/google-chrome-stable"
        root_dir: str = "/mnt/m/Documents/Comics"
    case "replit":
        chrome_options.add_argument("--no-sandbox")
        chrome_options.add_argument(
            "--user-data-dir=/home/runner/pyscan/.config/google-chrome"
        )
        chrome_options.binary_location = "/nix/store/k2rzszkc15aiyclc5p7063032jmg1dij-google-chrome-108.0.5359.94/bin/google-chrome-stable"
        root_dir: str = "/home/runner/pyscan/comics"


def newer_than(volume: int, chapter: str) -> Callable[[ReleaseInfo], bool]:
    return (
        lambda relinfo: float(relinfo.chapter) > float(chapter)
        if hasattr(relinfo, "chapter")
        else relinfo.volume > volume
        if hasattr(relinfo, "volume")
        else False
    )


# pylint: disable=anomalous-backslash-in-string
xml_http_request = """
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
      window.postMessage(JSON.stringify({
          magic: magic,
          url: url,
          id: input.id
      }), origin);
"""


def download_images(
    chrome: WebDriver, relinfo: ReleaseInfo, file_path: str, images: list
) -> bool:
    mkdir(file_path)
    if hasattr(relinfo, "chapter"):
        relinfo = f"{gray}chapter{reset} {yellow}{relinfo.chapter}{reset}"
    else:
        relinfo = f"{gray}volume{reset} {yellow}{relinfo.volume}{reset}"
    downloading = f"{gray}downloading{reset} {relinfo}"
    left_bracket = f"{gray}[{reset}"
    print(downloading, left_bracket, end="", flush=True)
    success = True
    if len(images) == 0:
        print(f"{red}Error: Images not found{reset}")
        success = False
    else:
        for n, image_url in enumerate(images, start=1):
            done = False
            data_uri = chrome.execute_async_script(xml_http_request, image_url)
            data = parse_data_uri(data_uri)
            base = str(n).zfill(3)
            ext = data.media_type.partition("/")[2]
            if ext == "jpeg":
                ext = "jpg"
            file_name = f"{base}.{ext}"
            tick = str(n) if n % 10 == 0 else "|" if n % 5 == 0 else "·"
            with open("/".join([file_path, file_name]), "wb") as file:
                file.write(data.data)
                done = True
            if done:
                color = f"{green}"
            else:
                color = f"{red}"
                success = False
            print(f"{color}{tick}{reset}", end="", flush=True)
    right_bracket = f"{gray}]{reset}"
    print(right_bracket)
    return success


def make_chapter_dirname(relinfo: ReleaseInfo) -> str:
    if hasattr(relinfo, "chapter"):
        splits = [*relinfo.chapter.partition(".")]
        splits[0] = splits[0].zfill(3)
        relinfo_dir = "".join(splits)
    elif hasattr(relinfo, "volume"):
        relinfo_dir = str(relinfo.volume).zfill(2)
    return relinfo_dir


class ChromeDriver:
    def __init__(self, options) -> None:
        self.options = options
        self.chrome = webdriver.Chrome(options=options)

    def __enter__(self) -> WebDriver:
        return self.chrome

    def __exit__(self, *args) -> None:
        self.chrome.quit()


def update_comics_table(con: Connection, comic: ComicInfo, relinfo: ReleaseInfo) -> int:
    cur = con.cursor()
    query = """
        UPDATE comics
        SET {relinfo} = ?
        WHERE comic = ?;
  """
    if hasattr(relinfo, "chapter"):
        cur.execute(query.format(relinfo="chapter"), [relinfo.chapter, comic.comic])
    elif hasattr(relinfo, "volume"):
        cur.execute(query.format(relinfo="volume"), [relinfo.volume, comic.comic])
    con.commit()
    return cur.rowcount


def update_webs_table(con: Connection, web: int, sentinel: str) -> int:
    cur = con.cursor()
    query = """
        UPDATE webs
        SET sentinel = ?
        WHERE web = ?;
  """
    cur.execute(query, [sentinel, web])
    con.commit()
    return cur.rowcount


def download_chapter(
    chrome: WebDriver, web_info: WebInfo, comic: ComicInfo, chapter: ReleaseInfo
) -> bool:
    chrome.get(chapter.url)
    wait_until(chrome, web_info.is_loaded)
    chap_dir = make_chapter_dirname(chapter)
    file_path = "/".join([root_dir, comic.folder, chap_dir])
    images = chrome.execute_script(web_info.scrape_images)
    return download_images(chrome, chapter, file_path, images)


def download_comic(
    con: Connection,
    chrome: WebDriver,
    web_info: WebInfo,
    comic: ComicInfo,
) -> None:
    newer_than_comic = newer_than(comic.volume, comic.chapter)
    chrome.get(comic.url)
    wait_until(chrome, web_info.is_loaded)
    chapters = chrome.execute_script(web_info.scrape_chapters)
    all_releases = map(lambda row: ReleaseInfo(row), chapters)
    new_chapters = [*filter(newer_than_comic, all_releases)]
    if len(new_chapters) == 0:
        print(f"{gray}scanning{reset} {comic.info} {green}is up to date{reset}")
        return True
    else:
        success = True
        print(f"{gray}scanning{reset} {comic.info}")
        for chapter in new_chapters:
            if download_chapter(chrome, web_info, comic, chapter):
                if update_comics_table(con, comic, chapter) != 1:
                    print(f"{red}Error: Cannot update comics table{reset}")
            else:
                success = False
        return success


def scanweb(webs: list[int]) -> None:
    con = sqlite3.connect(db_file)
    web_infos = get_web_infos(con)
    colorama_init()
    with ChromeDriver(chrome_options) as chrome:
        for web in webs:
            all_comics = []
            sentinel = 0
            for page in [*range(first_page, last_page + 1)]:
                web_info = web_infos[web]
                domain = f"{blue}{web_info.domain}{reset}"
                page_no = f"{gray}[{page}]{reset}"
                print(domain, page_no)
                urlpath = chrome.execute_script(web_info.gen_url, page)
                chrome.get(f"https://{web_info.domain}{urlpath}")
                wait_until(chrome, web_info.is_loaded)
                comics = chrome.execute_script(web_info.scrape_comics)
                pred = lambda comic: comic.url != web_info.sentinel
                releases = map(lambda row: ReleaseInfo(row), comics)
                new_comics = [*takewhile(pred, releases)]
                all_comics = all_comics + new_comics
                if len(new_comics) > 0:
                    for comic in query_comics(con, new_comics):
                        release = [r for r in new_comics if r.url == comic.url]
                        if not newer_than(comic.volume, comic.chapter)(release[0]):
                            print(f"{yellow}{comic.title}{reset} {green}is up to date{reset}")
                            continue
                        if not download_comic(con, chrome, web_info, comic):
                            for i, elem in enumerate(all_comics):
                                if elem.url == comic.url:
                                    sentinel = i + 1
                                    break
                if len(new_comics) != len(comics):
                    break
            if len(all_comics) > 0:
                if update_webs_table(con, web, all_comics[sentinel].url) != 1:
                    print(f"{red}Error: Cannot update webs table{reset}")


def rm_comic(comic: int) -> int:
    con = sqlite3.connect(db_file)
    con.execute("PRAGMA foreign_keys = 1")
    cur = con.cursor()
    delete_comic = "DELETE FROM comics WHERE comic=?;"
    cur.execute(delete_comic, [comic])
    con.commit()
    if cur.rowcount == 1:
        shift_up = "UPDATE comics SET comic=-(comic-1) WHERE comic>?;"
        cur.execute(shift_up, [comic])
        negate_minus_id = "UPDATE comics SET comic=-comic WHERE comic<0;"
        cur.execute(negate_minus_id)
        con.commit()
    con.close()
    return cur.rowcount


first_page: int = 1
last_page: int = 20
db_file: str = "scanweb.sqlite3"
max_timeout: int = 1000 * 30  # milliseconds

mangaraw_so: int = 0
mangaraw_io: int = 1
manga1001_su: int = 2
weloma_art: int = 3
welovemanga_one: int = 4
klmanga_net: int = 5
hachimanga_com: int = 6
j8jp_com: int = 7

list_of_webs: list[int] = [
    # mangaraw_so,
    # mangaraw_io,
    # manga1001_su,
    weloma_art,
    welovemanga_one,
    klmanga_net,
    # hachimanga_com,
    # j8jp_com,
 ]


if __name__ == "__main__":
    scanweb(list_of_webs)
