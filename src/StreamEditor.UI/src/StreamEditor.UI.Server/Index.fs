module StreamEditor.UI.Server.Index

open Bolero.Html
open Bolero.Server.Html
open StreamEditor.UI

let page =
    doctypeHtml {
        head {
            meta { attr.charset "UTF-8" }

            meta {
                attr.name "viewport"
                attr.content "width=device-width, initial-scale=1.0"
            }

            title { "Bolero Application" }
            ``base`` { attr.href "/" }

            link {
                attr.rel "stylesheet"
                attr.href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.4/css/bulma.min.css"
            }

            link {
                attr.rel "stylesheet"
                attr.href "css/index.css"
            }
        }

        body {
            attr.``class`` "dark-theme"

            nav {
                attr.``class`` "navbar is-black"
                "role" => "navigation"
                attr.aria "label" "main navigation"

                div {
                    attr.``class`` "navbar-brand"

                    a {
                        attr.``class`` "navbar-item has-text-weight-bold is-size-5"
                        attr.href "https://fsbolero.io"

                        img {
                            attr.style "height:40px"
                            attr.src "https://github.com/fsbolero/website/raw/master/src/Website/img/wasm-fsharp.png"
                        }

                        "  Bolero"
                    }
                }
            }

            div {
                attr.id "main"
                comp<Client.Main.MyApp>
            }

            boleroScript
            script { attr.src "focus.js" }
        }
    }
