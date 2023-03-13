#!/usr/bin/env python3

from colorama import Fore
from colorama import Style
from colorama import init as colorama_init
from itertools import takewhile
from os import mkdir
from pathlib import Path
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.chrome.webdriver import WebDriver
from sqlite3 import Connection, Cursor
from typing import Callable
from typing import List
from w3lib.url import ParseDataURIResult, parse_data_uri
import os
import sqlite3
import subprocess
import sys


blue: str = f"{Fore.BLUE}"
gray: str = f"{Fore.BLACK}{Style.BRIGHT}"
green: str = f"{Fore.GREEN}"
red: str = f"{Fore.RED}"
yellow: str = f"{Fore.YELLOW}"
reset: str = f"{Style.RESET_ALL}"


script_path: str = os.path.dirname(os.path.realpath(__file__))

get_comic_infos_sql: str = Path(script_path + "/sql/get_comic_infos.sql").read_text()

wait_until_js: str = Path(script_path + "/javascript/wait_until.js").read_text()
xml_http_request_js: str = Path(
    script_path + "/javascript/xml_http_request.js"
).read_text()


class ReleaseInfo:
    def __init__(self, record: dict):
        # chapter has priority over volume
        if "chapter" in record:
            self.chapter: str = record["chapter"]
        elif "volume" in record:
            self.volume: int = record["volume"]
        self.url: str = record["url"]


class WebInfo:
    def __init__(self, record: tuple):
        web_index: int = 0
        domain_index: int = 1
        user_name_index: int = 2
        password_index: int = 3
        sentinel_index: int = 4
        gen_url_index: int = 5
        is_loaded_index: int = 6
        scrape_comics_index: int = 7
        scrape_latest_index: int = 8
        scrape_chapters_index: int = 9
        scrape_images_index: int = 10
        self.web: int = record[web_index]
        self.domain: str = record[domain_index]
        self.user_name: str or None = record[user_name_index]
        self.password: str or None = record[password_index]
        self.sentinel: str = record[sentinel_index]
        self.gen_url: str = record[gen_url_index]
        self.is_loaded: str = record[is_loaded_index]
        self.scrape_comics: str = record[scrape_comics_index]
        self.scrape_latest: str = record[scrape_latest_index]
        self.scrape_chapters: str = record[scrape_chapters_index]
        self.scrape_images: str = record[scrape_images_index]


class ComicInfo:
    def __init__(self, record: tuple, new_release: ReleaseInfo = None):
        comic_index: int = 0
        url_index: int = 1
        title_index: int = 2
        folder_index: int = 3
        volume_index: int = 4
        chapter_index: int = 5
        self.comic: int = record[comic_index]
        self.url: str = record[url_index]
        self.title: str = record[title_index]
        self.folder: str = record[folder_index]
        self.volume: int = record[volume_index]
        self.chapter: str = record[chapter_index]
        self.new_release: ReleaseInfo = new_release
        self.info: str = (
            f"{yellow}{self.title} {gray}(Vol.{self.volume}, Ch.{self.chapter}){reset}"
        )


# For bracket-style exception handling
class ChromeDriver:
    def __init__(self, options) -> None:
        self.options: Options = options
        self.chrome: WebDriver = webdriver.Chrome(options=options)

    def __enter__(self) -> WebDriver:
        return self.chrome

    def __exit__(self, *args) -> None:
        self.chrome.quit()


def get_web_infos(con: Connection) -> dict[int, WebInfo]:
    query: str = """ SELECT * FROM webs; """
    cur: Cursor = con.cursor()
    result: Cursor = cur.execute(query)
    return dict(map(lambda row: (row[0], WebInfo(row)), result.fetchall()))


def get_comic_infos(con: Connection, releases: list[ReleaseInfo]) -> map:
    values: str = ",".join([f"({i},?)" for i, _ in enumerate(releases)])
    query: str = get_comic_infos_sql.format(values=values)
    # con.set_trace_callback(print)
    cur: Cursor = con.cursor()
    result: Cursor = cur.execute(query, [*map(lambda release: release.url, releases)])
    return map(lambda row: ComicInfo(row), result.fetchall())


def wait_until(chrome: WebDriver, pred: str, timeout: int = -1) -> None:
    if timeout == -1:
        timeout = max_timeout
    chrome.execute_script(wait_until_js, pred, timeout)


def newer_than(volume: int, chapter: str) -> Callable[[ReleaseInfo], bool]:
    return (
        lambda relinfo: float(relinfo.chapter) > float(chapter)
        if hasattr(relinfo, "chapter")
        else relinfo.volume > volume
        if hasattr(relinfo, "volume")
        else False
    )


def download_images(
    chrome: WebDriver, relinfo: ReleaseInfo, file_path: str, images: list[str]
) -> bool:
    mkdir(file_path)
    relinfo: str = (
        f"{gray}chapter{reset} {yellow}{relinfo.chapter}{reset}"
        if hasattr(relinfo, "chapter")
        else f"{gray}volume{reset} {yellow}{relinfo.volume}{reset}"
    )
    downloading: str = f"{gray}downloading{reset} {relinfo}"
    left_bracket: str = f"{gray}[{reset}"
    print(downloading, left_bracket, end="", flush=True)
    success: bool = True
    if len(images) == 0:
        print(f"{red}Error: Images not found{reset}")
        success = False
    else:
        for n, image_url in enumerate(images, start=1):
            done: bool = False
            data_uri: str = chrome.execute_async_script(xml_http_request_js, image_url)
            data: ParseDataURIResult = parse_data_uri(data_uri)
            base: str = str(n).zfill(3)
            ext: str = data.media_type.partition("/")[2]
            if ext == "jpeg":
                ext = "jpg"
            file_name: str = f"{base}.{ext}"
            tick: str = str(n) if n % 10 == 0 else "|" if n % 5 == 0 else "·"
            with open("/".join([file_path, file_name]), "wb") as file:
                file.write(data.data)
                done = True
            color: str = f"{green}" if done else f"{red}"
            if not done:
                success = False
            print(f"{color}{tick}{reset}", end="", flush=True)
    right_bracket: str = f"{gray}]{reset}"
    print(right_bracket)
    return success


def make_chapter_dirname(relinfo: ReleaseInfo) -> str:
    if hasattr(relinfo, "chapter"):
        splits: list[str] = [*relinfo.chapter.partition(".")]
        splits[0] = splits[0].zfill(3)
    relinfo_dir: str = (
        "".join(splits)
        if hasattr(relinfo, "chapter")
        else str(relinfo.volume).zfill(2)
        if hasattr(relinfo, "volume")
        else "000"
    )
    return relinfo_dir


def update_comics_table(con: Connection, comic: ComicInfo, relinfo: ReleaseInfo) -> int:
    cur: Cursor = con.cursor()
    query: str = """
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
    cur: Cursor = con.cursor()
    query: str = """
        UPDATE webs
        SET sentinel = ?
        WHERE web = ?;
  """
    cur.execute(query, [sentinel, web])
    con.commit()
    return cur.rowcount


def download_chapter(
    chrome: WebDriver,
    web_info: WebInfo,
    comic: ComicInfo,
    chapter: ReleaseInfo,
    root_dir: str,
) -> bool:
    chrome.get(chapter.url)
    wait_until(chrome, web_info.is_loaded)
    chap_dir: str = make_chapter_dirname(chapter)
    file_path: str = "/".join([root_dir, comic.folder, chap_dir])
    images: list[str] = chrome.execute_script(web_info.scrape_images)
    return download_images(chrome, chapter, file_path, images)


def download_comic(
    con: Connection,
    chrome: WebDriver,
    web_info: WebInfo,
    comic: ComicInfo,
    root_dir: str,
) -> None:
    newer_than_comic: Callable[[ReleaseInfo], bool] = newer_than(
        comic.volume, comic.chapter
    )
    chrome.get(comic.url)
    wait_until(chrome, web_info.is_loaded)
    chapters: list[tuple] = chrome.execute_script(web_info.scrape_chapters)
    all_releases: list[ReleaseInfo] = map(lambda row: ReleaseInfo(row), chapters)
    new_chapters: list[ReleaseInfo] = [*filter(newer_than_comic, all_releases)]
    if len(new_chapters) == 0:
        print(f"{gray}scanning{reset} {comic.info} {green}is up to date{reset}")
        return True
    else:
        success: bool = True
        print(f"{gray}scanning{reset} {comic.info}")
        for chapter in new_chapters:
            if download_chapter(chrome, web_info, comic, chapter, root_dir):
                if update_comics_table(con, comic, chapter) != 1:
                    print(f"{red}Error: Cannot update comics table{reset}")
            else:
                success = False
        return success


def scanweb(chrome: WebDriver, webs: list[int], root_dir: str) -> None:
    con: Connection = sqlite3.connect("/".join([root_dir, db_file]))
    web_infos: list[WebInfo] = get_web_infos(con)
    colorama_init()
    for web in webs:
        all_comics: list[ReleaseInfo] = []
        sentinel: int = 0
        for page in [*range(first_page, last_page + 1)]:
            web_info: WebInfo = web_infos[web]
            domain: str = f"{blue}{web_info.domain}{reset}"
            page_no: str = f"{gray}[{page}]{reset}"
            print(domain, page_no)
            urlpath: str = chrome.execute_script(web_info.gen_url, page)
            chrome.get(f"https://{web_info.domain}{urlpath}")
            wait_until(chrome, web_info.is_loaded)
            comics: list[tuple] = chrome.execute_script(web_info.scrape_comics)
            pred: Callable[[ReleaseInfo], bool] = (
                lambda comic: comic.url != web_info.sentinel
            )
            releases: list[ReleaseInfo] = map(lambda row: ReleaseInfo(row), comics)
            new_comics: list[ReleaseInfo] = [*takewhile(pred, releases)]
            all_comics = all_comics + new_comics
            if len(new_comics) > 0:
                for comic in get_comic_infos(con, new_comics):
                    release: list[ReleaseInfo] = [
                        r for r in new_comics if r.url == comic.url
                    ]
                    newer_than_comic: Callable[[ReleaseInfo], bool] = newer_than(
                        comic.volume, comic.chapter
                    )
                    if len(release) > 0 and not newer_than_comic(release[0]):
                        print(
                            f"{yellow}{comic.title}{reset} {green}is up to date{reset}"
                        )
                        continue
                    if not download_comic(con, chrome, web_info, comic, root_dir):
                        for i, elem in enumerate(all_comics):
                            if elem.url == comic.url:
                                sentinel = i + 1
                                break
            if len(new_comics) != len(comics):
                break
        if len(all_comics) > 0:
            if update_webs_table(con, web, all_comics[sentinel].url) != 1:
                print(f"{red}Error: Cannot update webs table{reset}")


def rm_comic_aux(comic: int, root_dir: str) -> None:
    con: Connection = sqlite3.connect("/".join([root_dir, db_file]))
    con.execute("PRAGMA foreign_keys = 1")
    cur: Cursor = con.cursor()
    delete_comic: str = "DELETE FROM comics WHERE comic=?;"
    cur.execute(delete_comic, [comic])
    con.commit()
    if cur.rowcount == 1:
        shift_up: str = "UPDATE comics SET comic=-(comic-1) WHERE comic>?;"
        cur.execute(shift_up, [comic])
        negate_minus_id: str = "UPDATE comics SET comic=-comic WHERE comic<0;"
        cur.execute(negate_minus_id)
        con.commit()
    con.close()


def new_comic_at_aux(comic: int, root_dir: str) -> None:
    con: Connection = sqlite3.connect("/".join([root_dir, db_file]))
    con.execute("PRAGMA foreign_keys = 1")
    cur: Cursor = con.cursor()
    shift_down: str = "UPDATE comics SET comic=-(comic+1) WHERE comic>=?;"
    cur.execute(shift_down, [comic])
    negate_minus_id: str = "UPDATE comics SET comic=-comic WHERE comic<0;"
    cur.execute(negate_minus_id)
    con.commit()


def get_root_dir(config: str) -> str:
    if config == "wsl":
        root_dir = "/mnt/m/Documents/Comics"
    elif config == "replit":
        root_dir = "/home/runner/pyscan/comics"
    else:
        raise Exception('Unknown configuration. Expecting "wsl" or "replit".')
    return root_dir


def get_chrome_options(config: str) -> Options:
    chrome_options: Options = webdriver.ChromeOptions()
    user_data_dir: str = os.getenv("HOME")
    if config == "replit":
        user_data_dir += "/" + os.getenv("REPL_SLUG")
    user_data_dir += "/.config/google-chrome"
    if config == "replit":
        chrome_options.add_argument("--no-sandbox")
    chrome_options.add_argument(f"--user-data-dir={user_data_dir}")
    chrome_options.binary_location = (
        subprocess.run(["which", "google-chrome-stable"], stdout=subprocess.PIPE)
        .stdout.decode("utf-8")
        .strip()
    )
    return chrome_options


configs: list[str] = ["wsl", "replit"]
config: str = configs[0]

first_page: int = 1
last_page: int = 30
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


def rm_comic(comic: int) -> None:
    rm_comic_aux(comic, get_root_dir(config))

def new_comic_at(comic: int) -> None:
    new_comic_at_aux(comic, get_root_dir(config))


if __name__ == "__main__":
    chrome_options: Options = get_chrome_options(config)
    with ChromeDriver(chrome_options) as chrome:
        root_dir: str = get_root_dir(config)
        scanweb(chrome, list_of_webs, root_dir)
