import { Elm } from "./elm/src/Main.elm"
window.customElements.define("completion-boundary",
    class Boundary extends HTMLElement {
        constructor() {
            super()
            this.elm = null
            this.attachShadow({ mode: "open" })
            this.shadowRoot.appendChild(document.createElement("div"))
        }
        connectedCallback() {
            this.style.height = "100%"
            this.style.width = "100%"
            const plantId = this.getAttribute("plantId")
            if (plantId) {
                this.initializeElm(plantId)
            } else {
                this.shadowRoot.firstChild.innerText = "Error: plantId not provided!"
            }
            window.addEventListener("boundary:got-token", function (e) { this.gotToken(e) }.bind(this), false)
        }
        disconnectedCallback() {
            window.removeEventListener("boundary:got-token", this.gotToken)
        }
        gotToken(e) {
            this.elmMsg("gotToken", e.detail)
        }
        initializeElm(plantId) {
            this.elm = Elm.Main.init({
                node: this.shadowRoot.firstChild
                , flags: { plantId }
            })
            this.elm.ports.toJs.subscribe(msg => this.update(msg))
        }
        elmMsg(topic, payload) {
            this.elm.ports.fromJs.send({ topic: topic, payload: payload ? payload : null })
        }
        update(msg) {
            switch (msg.topic) {
                case "get": this.get(msg.payload); break;
                case "put": this.put(msg.payload); break;
                case "getToken": this.event("token-requested", { pluginName: "boundary", request: msg.payload }); break;
                default: console.log("unknown msg from elm: ", msg)
            }
        }
        event(topic, payload) {
            this.dispatchEvent(new CustomEvent(topic, { bubbles: true, detail: payload }))
        }
        get(key) {
            openDb().then(db => {
                db.transaction("boundary", "readonly")
                    .objectStore("boundary")
                    .get(key)
                    .onsuccess = (e) => { this.elmMsg("got", { key: key, data: e.target.result }) }
            })
        }
        put({ key, payload }) {
            openDb().then(db => {
                db.transaction("boundary", "readwrite")
                    .objectStore("boundary")
                    .put(payload, key)
            })
        }
    })


function openDb() {
    return new Promise((resolve, reject) => {
        const request = indexedDB.open("completion-boundary", 1);
        request.onupgradeneeded = onDbupgradeneeded()
        request.onsuccess = () => resolve(request.result);
        request.onerror = () => reject(request.error);
        request.onblocked = () => console.log("DB Blocked!");
    });
}

function onDbupgradeneeded() {
    return function (event) {
        let db = event.target.result
        if (db.objectStoreNames.contains("boundary")) {
            //Do nothing
        } else {
            db.createObjectStore("boundary")
        }
    }
}