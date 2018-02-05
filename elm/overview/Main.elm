module Main exposing (..)

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Options as Options exposing (css, when)
import Material.Layout as Layout
import Material.Card as Card
import Material.Icon as Icon
import Material.Chip as Chip
import Material.Textfield as Textfield
import Material.Elevation as Elevation
import Material.Color as Color
import Material.Tooltip as Tooltip
import Material.Grid as Grid exposing (grid, cell, size, Device(..))
import Date
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Format as Format exposing (format, formatUtc, isoMsecOffsetFormat)
import Markdown
import String
import Dom
import Task


-- TYPES


type alias Mdl =
    Material.Model


type alias Brownbag =
    { title : String
    , description : String
    , available : Bool
    , filename : String
    }



-- MODEL


type alias Model =
    { mdl : Material.Model
    , brownbags : List Brownbag
    , query : String
    , searchOpen : Bool
    }


model : Model
model =
    { mdl = Material.model
    , brownbags =
        [ { title = "RxJS"
          , filename = "index-rxjs.html"
          , available = True
          , description = """
Talk for beginners that introduces [RxJS (Reactive Extensions for Javascript)](http://reactivex.io/rxjs/) developed by Microsoft.

This presentation covers:
* Introduction to functional and reactive programming
* Observables API discussion
* Marble diagrams
* Comparison of cold and hot Observables
* Combining Observables with operators
* Error handling
* Observables in Angular 2
* Outlook: Standardization process of Observables

All examples given in this talk are written in RxJS 5.
"""
          }
        , { title = "TypeScript - Automated codestyle"
          , available = True
          , description = """
Talk that shows how to easily setup an TypeScript enviroment with automated linting and code-style conventions.
"""
          , filename = "index-typescript-automated-codestyle.html"
          }
        ]
    , query = ""
    , searchOpen = False
    }



-- UPDATE


type Msg
    = Mdl (Material.Msg Msg)
      -- Boilerplate: Msg clause for internal Mdl messages.
    | NoOp
    | UpdateSearchQuery String
    | OpenOrClearSearch
    | MaybeResetSearch
    | ResetSearch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Boilerplate: Mdl action handler.
        Mdl msg' ->
            Material.update msg' model

        NoOp ->
            model ! []

        UpdateSearchQuery newQuery ->
            { model | query = newQuery } ! []

        OpenOrClearSearch ->
            let
                newSearchOpen =
                    not model.searchOpen

                newQuery =
                    if newSearchOpen == True then
                        model.query
                    else
                        ""

                focus =
                    Dom.focus "brownbag-search"

                cmd =
                    if newSearchOpen == True then
                        Task.perform (\_ -> NoOp) (\_ -> NoOp) focus
                    else
                        Cmd.none
            in
                ( { model | searchOpen = newSearchOpen, query = newQuery }, cmd )

        MaybeResetSearch ->
            let
                newSearchOpen =
                    String.length model.query > 0
            in
                { model | searchOpen = newSearchOpen } ! []

        ResetSearch ->
            { model | searchOpen = False, query = "" } ! []


filterBrownbags : String -> Brownbag -> Bool
filterBrownbags query brownbag =
    (String.contains (String.toLower query) (String.toLower brownbag.title))
        || (String.contains (String.toLower query) (String.toLower brownbag.description))



-- VIEW


type alias Page =
    { title : String
    , content : Html Msg
    , githubUser : String
    , query : String
    , searchOpen : Bool
    , countResults : Int
    }


type alias Card =
    { title : String
    , description : Html Msg
    , btnText : String
    , btnHref : String
    , accessable : Bool
    }


view : Model -> Html Msg
view model =
    let
        brownbags =
            (List.filter (filterBrownbags model.query) model.brownbags)

        cards =
            (List.map brownbagCardView brownbags)

        countResults =
            if String.length model.query > 0 then
                List.length cards
            else
                -1

        page =
            { title = "Brownbags"
            , content =
                grid []
                    (List.map (\card -> cell [ Grid.size Desktop 4, Grid.size Phone 4 ] [ card ]) cards)
            , githubUser = "dotcs"
            , query = model.query
            , searchOpen = model.searchOpen
            , countResults = countResults
            }
    in
        Layout.render Mdl
            model.mdl
            []
            { header = header page
            , drawer = []
            , tabs = ( [], [] )
            , main =
                [ div [ class "page-content" ] (content page) ]
            }


header : Page -> List (Html Msg)
header page =
    [ Layout.row []
        [ Layout.title []
            [ text page.title
            , span [ class "title-author" ] [ text (" by " ++ page.githubUser) ]
            ]
        , Layout.spacer
        , Button.render Mdl
            [ 0 ]
            model.mdl
            [ Button.icon
            , Button.onClick OpenOrClearSearch
            ]
            [ Icon.view
                (if page.searchOpen then
                    "clear"
                 else
                    "search"
                )
                []
            ]
        , Textfield.render Mdl
            [ 0 ]
            model.mdl
            [ Textfield.floatingLabel
            , Textfield.value page.query
            , Textfield.onInput UpdateSearchQuery
            , Textfield.onBlur MaybeResetSearch
            , Options.inner [ Options.id "brownbag-search" ]
            , (css "display" "none") `when` (not page.searchOpen)
            , (css "width" "200px")
            ]
        , Layout.navigation []
            [ Layout.link [ Layout.href ("https://github.com/" ++ String.toLower page.githubUser) ]
                [ Button.render Mdl
                    [ 0 ]
                    model.mdl
                    [ Button.icon
                    ]
                    [ i [ class "octicon octicon-mark-github", title page.githubUser ] [] ]
                ]
            ]
        ]
    ]


content : Page -> List (Html Msg)
content page =
    [ if page.countResults > -1 then
        div [ class "search-result-info" ]
            [ Chip.span []
                [ Chip.contact Html.span
                    [ Color.background Color.primary
                    , Color.text Color.white
                    ]
                    [ span [ class "mdlx-chip--icon" ] [ Icon.i "search" ] ]
                , Chip.content []
                    [ text (toString page.countResults ++ " results for '" ++ page.query ++ "'") ]
                ]
            ]
      else
        text ""
    , page.content
    ]


brownbagCardView : Brownbag -> Html Msg
brownbagCardView brownbag =
    cardView
        { title = brownbag.title
        , description = Markdown.toHtml [] brownbag.description
        , btnText = "Show presentation"
        , btnHref = brownbag.filename
        , accessable = brownbag.available
        }


cardView : Card -> Html Msg
cardView card =
    let
        menuItems = []

        actionItems =
            [ if card.accessable then
                Layout.link [ Layout.href card.btnHref ]
                    [ Button.render Mdl
                        [ 0 ]
                        model.mdl
                        [ Button.ripple, Button.accent ]
                        [ text card.btnText ]
                    ]
              else
                Button.render Mdl
                    [ 0 ]
                    model.mdl
                    [ Button.disabled ]
                    [ text card.btnText ]
            ]
    in
        Card.view [ Elevation.e2 ]
            [ Card.title [] [ Card.head [] [ text card.title ] ]
            , Card.text [] [ card.description ]
            , Card.menu [] menuItems
            , Card.actions [ Card.border ] actionItems
            ]


main : Program Never
main =
    App.program
        { init = ( model, Material.init Mdl )
        , view = view
        , update = update
        , subscriptions = Material.subscriptions Mdl
        }
