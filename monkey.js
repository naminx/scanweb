// ==UserScript==
// @name         Syosetu: Store images
// @namespace    https://github.com/naminx
// @version      0.1
// @description  Nyaa: Unhide long text
// @author       You
// @match        https://manga9.co/chapters/*
// @match        https://mangaraw.co/chapters/*
// @match        https://mangaraw.vip/chapters/*
// @match        https://mangarawjp.com/chapters/*
// @match        https://syosetu.me/chapters/*
// @match        https://rawdevart.com/comic/*
// @match        https://weloma.art/*
// @match        https://welovemanga.one/*
// @match        https://klmanga.net/*
// @match        https://klmanga.com/*
// @match        https://mangahatachi.com/*
// @match        https://j9jp.com/*
// @icon         https://syosetu.me/images/android-chrome-192x192.png
// @grant        GM.xmlHttpRequest
// @connect      blazecloud.org
// @connect      blazeclouds.co
// @connect      byteblaze.me
// @connect      cloudblaze.org
// @connect      justaquickbite.com
// @connect      manga9.co
// @connect      megabyteblaze.me
// @connect      rawdevart.com
// @connect      welovekai.com
// @connect      ihlv1.xyz
// @connect      mm2r.net

// ==/UserScript==

//--- Install event listener on "message".
//--- The sender can send message using the following code:
//---
//--- window.postMessage (JSON.stringify(data), targetOrigin);
//---
//--- where `targetOrigin` is "https://syosetu.me/" etc.

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
    new Promise((resolve, reject) =>
        GM.xmlHttpRequest({
            method: "GET",
            headers: {
                referer: referer,
                accept: "image/webp;q=0.9, */*;q=0.8",
            },
            url: url,
            withCredentials: true,
            responseType: "blob",
            onload: (httpRequestResult) => {
                const fileReader = new FileReader();
                fileReader.onloadend = () => resolve(fileReader.result);
                fileReader.readAsDataURL(httpRequestResult.response);
            },
            onerror: (httpRequestResult) =>
                reject(new Error(JSON.stringify(httpRequestResult))),
        })
    );

//--- This event handler listens for the right kind of message and calls
//--- "GM.xmlhttpRequest". The message shall be the following JSON object:
//---     { magic: "xmlHttpRequest",
//---       url: "https://...",
//---       id: "..." ]
//--- "magic" must be "xmlHttpRequest", or else the handler does nothing.
//--- "id" is an `id` of the element to store the result. If an element with
//--- such `id` does not exist, the handler exits immediately.
//--- If everything goes well, the handler sets "value" field of the element
//--- to the content in base64 encoding with MIME type, and triggers an
//--- event with the same name as the `id` on the element.

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
