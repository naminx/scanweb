// ==UserScript==
// @name         MangaRaw: xmlHttpRequest
// @namespace    https://github.com/naminx
// @version      0.1
// @description  MangaRaw: xmlHttpRequest
// @author       You
// @match        https://hachimanga.com/*
// @match        https://j8jp.com/*
// @match        https://j9jp.com/*
// @match        https://klmanga.com/*
// @match        https://klmanga.net/*
// @match        https://manga1001.in/chapters/*
// @match        https://manga1001.su/chapters/*
// @match        https://mangagun.com/*
// @match        https://mangahatachi.com/*
// @match        https://mangaraw.co/chapters/*
// @match        https://mangaraw.io/chapters/*
// @match        https://mangaraw.so/chapters/*
// @match        https://mangaraw.vip/chapters/*
// @match        https://mangarawjp.com/chapters/*
// @match        https://rawdevart.com/comic/*
// @match        https://syosetu.me/chapters/*
// @match        https://weloma.art/*
// @match        https://welovemanga.one/*
// @icon         https://manga1001.in/favicon.ico
// @grant        GM.xmlHttpRequest
// @connect      asia-s10.bunnycdn.org
// @connect      blazecloud.org
// @connect      blazeclouds.co
// @connect      byteblaze.me
// @connect      cdnv2.shonenjumpmangas.com
// @connect      cloudblaze.co
// @connect      cloudblaze.org
// @connect      comick.top
// @connect      hachimanga.com
// @connect      ihlv1.xyz
// @connect      j9.phoenixmanga.com
// @connect      j9jp.com
// @connect      justaquickbite.com
// @connect      klimv1.xyz
// @connect      manga1000.top
// @connect      mangahatachi.com
// @connect      megabyteblaze.me
// @connect      mg9.lectormanga.top
// @connect      mm2r.net
// @connect      rawdevart.com
// @connect      welovekai.com

// ==/UserScript==

//--- Install event listener on "message".
//--- The sender can send message using the following code:
//---
//--- window.postMessage (JSON.stringify(data), targetOrigin);
//---
//--- where `targetOrigin` is "https://mangaraw.io/" etc.

(() => {
    "use strict";
    window.addEventListener("message", getXmlHttpRequest, false);
})();

//--- This two lines are needed to build `referer`.
//--- `referer: "https://syosetu.me/"` etc is needed for some sites.

const location = window.location;

const referer = location.protocol + "//" + location.hostname + "/";

//--- This is the main function. If webdriver's `executeScript` can directly
//--- call this function, life will be much easier.

const getDataAsBase64 = (url) =>
    new Promise((resolve, _reject) =>
        GM.xmlHttpRequest({
            method: "GET",
            headers: {
                referer: referer,
                accept: "text/html,application/xhtml+xml,application/xml;q=0.9,"
                      + "image/avif,image/webp,image/apng,*/*;q=0.8,"
                      + "application/signed-exchange;v=b3;q=0.9"
            },
            url: url.replace("newimage.php?manga=", "proxy.php?link="),
            withCredentials: true,
            responseType: "blob",
            onload: (httpRequestResult) => {
                const fileReader = new FileReader();
                fileReader.onloadend = () => resolve(fileReader.result);
                fileReader.readAsDataURL(httpRequestResult.response);
            },
            onerror: (httpRequestResult) =>
                resolve(JSON.stringify(httpRequestResult)),
        })
    );

//--- This event handler listens for the right kind of message and calls
//--- "GM.xmlhttpRequest". The message shall be the following JSON object:
//---     { magic: "xmlHttpRequest"
//---     , url: "https://..."
//---     , id: "..."
//---     }
//--- "magic" must be "xmlHttpRequest", or else the handler does nothing.
//--- "id" is an `id` of an `input` element to store the result. If an element
//--- with such `id` does not exist, the handler exits immediately.
//--- If everything goes well, the handler sets "value" field of the element
//--- to the content in base64 encoding with MIME type, and fires an event
//--- with the same name as the `id` on the element.

async function getXmlHttpRequest(event) {
    try {
        const data = JSON.parse(event.data);
        if (data.magic == "xmlHttpRequest") {
            const input = document.getElementById(data.id);
            if (input != undefined) {
                input.value = await getDataAsBase64(data.url);
                input.dispatchEvent(new Event(data.id));
            }
        }
    } catch (zError) {
        // Do nothing
    }
}

