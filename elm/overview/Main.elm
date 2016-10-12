module Main exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Date
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Format as Format exposing (format, formatUtc, isoMsecOffsetFormat)
import Markdown
import String


type alias Model =
    { brownbags : List Brownbag
    , query : String
    , searchOpen : Bool
    }


type Presented
    = PresentedAt Date.Date
    | NotPresented


type alias Brownbag =
    { title : String
    , description : String
    , presentedAt : Presented
    , available : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { brownbags =
            [ { title = "RxJS"
              , presentedAt = PresentedAt (Date.fromString "2016/10/05" |> Result.withDefault (Date.fromTime 0))
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
            , { title = "Elm"
              , available = False
              , description = """
Introduction to Elm.
"""
              , presentedAt = NotPresented
              }
            ]
      , query = ""
      , searchOpen = False
      }
    , Cmd.none
    )


type Msg
    = MsgNothing
    | UpdateSearchQuery String
    | ToggleSearch
    | MaybeResetSearch
    | ResetSearch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgNothing ->
            ( model, Cmd.none )

        UpdateSearchQuery newQuery ->
            ( { model | query = newQuery }, Cmd.none )

        ToggleSearch ->
            let
                newSearchOpen =
                    not model.searchOpen

                newQuery =
                    if newSearchOpen then
                        model.query
                    else
                        ""
            in
                ( { model | searchOpen = newSearchOpen, query = newQuery }, Cmd.none )

        MaybeResetSearch ->
            let
                newSearchOpen =
                    String.length model.query > 0
            in
                ( { model | searchOpen = newSearchOpen }, Cmd.none )

        ResetSearch ->
            ( { model | searchOpen = False, query = "" }, Cmd.none )


filterBrownbags : String -> Brownbag -> Bool
filterBrownbags query brownbag =
    (String.contains (String.toLower query) (String.toLower brownbag.title))
        || (String.contains (String.toLower query) (String.toLower brownbag.description))


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
                div []
                    [ div [ class "mdl-grid" ]
                        (List.map (\card -> div [ class "mdl-cell mdl-cell--4-col" ] [ card ]) cards)
                    ]
            , githubUser = "dotcs"
            , query = model.query
            , searchOpen = model.searchOpen
            , countResults = countResults
            }
    in
        pageLayoutView page


type alias Page =
    { title : String
    , content : Html Msg
    , githubUser : String
    , query : String
    , searchOpen : Bool
    , countResults : Int
    }


pageLayoutView : Page -> Html Msg
pageLayoutView page =
    div [ class "mdl-layout mdl-js-layout mdl-layout--fixed-header" ]
        [ header [ class "mdl-layout__header" ]
            [ div [ class "mdl-layout__header-row" ]
                [ span [ class "mdl-layout-title" ]
                    [ text page.title
                    , span [ class "title-author" ] [ text (" by " ++ page.githubUser) ]
                    ]
                , div [ class "mdl-layout-spacer" ] []
                , div
                    [ classList
                        [ ( "mdl-textfield", True )
                        , ( "mdl-js-textfield", True )
                        , ( "mdl-textfield--expandable", True )
                        , ( "mdl-textfield-textfield--floating-label", True )
                        , ( "mdl-textfield--align-right", True )
                        , ( "is-focused", page.searchOpen )
                        , ( "is-dirty", (String.length page.query > 0) )
                        ]
                    ]
                    [ label [ class "mdl-button mdl-js-button mdl-button--icon", for "search-exp" ]
                        [ i [ class "material-icons", onClick ToggleSearch ] [ text "search" ]
                        ]
                    , div [ class "mdl-textfield__expandable-holder" ]
                        [ input
                            [ class "mdl-textfield__input"
                            , type' "text"
                            , name "sample"
                            , id "search-exp"
                            , value page.query
                            , onInput UpdateSearchQuery
                            , onBlur MaybeResetSearch
                            ]
                            []
                        ]
                    ]
                , nav [ class "mdl-navigation mdl-layout--large-screen-only" ]
                    [ a
                        [ class "mdl-navigation__link"
                        , href ("https://github.com/" ++ String.toLower page.githubUser)
                        ]
                        [ span [ class "mdl-button mdl-js-button mdl-button--icon" ]
                            [ i [ class "octicon octicon-mark-github", title page.githubUser ] []
                            ]
                        ]
                    ]
                ]
            ]
        , main' []
            [ div [ class "page-content" ]
                [ div [ classList [ ( "search-result-info", True ), ( "is-hidden", page.countResults < 0 ) ] ]
                    [ span [ class "mdl-chip mdl-chip--contact" ]
                        [ span [ class "mdl-chip__contact mdl-color--indigo mdl-color-text--white mdlx-chip--icon" ]
                            [ i [ class "material-icons", onClick ToggleSearch ] [ text "search" ]
                            ]
                        , span [ class "mdl-chip__text" ] [ text (toString page.countResults ++ " Results") ]
                        , span [ class "mdl-chip__action" ]
                            [ i [ class "material-icons", onClick ResetSearch ] [ text "cancel" ]
                            ]
                        ]
                    ]
                , page.content
                ]
            ]
        ]


brownbagCardView : Brownbag -> Html Msg
brownbagCardView brownbag =
    cardView
        { title = brownbag.title
        , description = Markdown.toHtml [] brownbag.description
        , btnText = "Show presentation"
        , btnHref = ("index-" ++ String.toLower brownbag.title ++ ".html")
        , accessable = brownbag.available
        , checked = not (brownbag.presentedAt == NotPresented)
        , checkedIconTooltip =
            case brownbag.presentedAt of
                PresentedAt date ->
                    Just ("presented at " ++ (format config config.format.date date))

                NotPresented ->
                    Nothing
        }


type alias Card =
    { title : String
    , description : Html Msg
    , btnText : String
    , btnHref : String
    , accessable : Bool
    , checked : Bool
    , checkedIconTooltip : Maybe String
    }


cardView : Card -> Html Msg
cardView card =
    div [ class "mdl-card mdl-shadow--2dp" ]
        [ div [ class "mdl-card__title" ]
            [ h2 [ class "mdl-card__title-text" ] [ text card.title ]
            ]
        , div [ class "mdl-card__supporting-text" ] [ card.description ]
        , if card.accessable then
            div [ class "mdl-card__actions mdl-card--border" ]
                [ a
                    [ class "mdl-button mdl-button--colored mdl-js-button mdl-js-ripple-effect"
                      -- , disabled (not card.accessable)
                    , href card.btnHref
                    ]
                    [ text card.btnText ]
                ]
          else
            text ""
        , if card.checked then
            div [ class "mdl-card__menu" ]
                [ button [ class "mdl-button mdl-button--icon mdl-js-button mdl-js-ripple-effect" ]
                    [ i [ class "material-icons", title (Maybe.withDefault "" card.checkedIconTooltip) ] [ text "check circle" ]
                    ]
                ]
          else
            text ""
        ]


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
