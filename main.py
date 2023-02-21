from selenium import webdriver
from selenium.webdriver.chrome.webdriver import WebDriver
import sqlite3
from sqlite3 import Connection

max_timeout = 1000 * 30

def get_webs_table(con: Connection) -> list:
    cur = con.cursor()
    res = cur.execute("""
        SELECT *
          FROM webs;
    """)
    return dict(map(lambda row: (row[0], row[1:]), res.fetchall()))

domain = 0
sentinel = 3
gen_url = 4
is_loaded = 5
scrape_comics = 6
scrape_latest = 7
scrape_chapters = 8
scrape_images = 9

def wait_until(browser: WebDriver, pred: str, timeout: int = max_timeout) -> None:
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
    browser.execute_script(js_wait_until, pred, timeout)

chrome_options = webdriver.ChromeOptions()
chrome_options.add_argument('--user-data-dir=/home/namin/.config/google-chrome')
chrome_options.binary_location = "/nix/store/4bp8w2xxx2b52dq43n1kwbhny8ja46w6-google-chrome-110.0.5481.77/bin/google-chrome-stable"

chrome = webdriver.Chrome(options=chrome_options)
list_of_webs = [3]
max_page = 3

con = sqlite3.connect("scanweb.sqlite3")
rows = get_webs_table(con)
for web in list_of_webs:
  for page in [*range(1, max_page + 1)]:
    result = chrome.execute_script(rows[web][gen_url], page)
    chrome.get('https://' + rows[web][domain] + result)
    wait_until(chrome, rows[web][is_loaded])
    result = chrome.execute_script(rows[web][scrape_comics])
    print(result)
