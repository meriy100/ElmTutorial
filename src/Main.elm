module Main exposing (main)

import Browser
import Page.Entries

main =
    Browser.element
        { init = Page.Entries.init
        , update = Page.Entries.update
        , subscriptions = Page.Entries.subscriptions
        , view = Page.Entries.view
        }
