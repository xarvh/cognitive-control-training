port module Main exposing (..)

import AboutView
import Html exposing (..)
import Html.App
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Navigation
import Pasat
import PasatView
import Task
import Time
import Wells



--
-- MODEL
--


type Page
    = About
    | Numbers
    | Birds

pages =
    [ About, Birds, Numbers ]


type alias Model =
    { page : Page
    , pasat : Pasat.Model
    , wells : Wells.Model
    }


type Action
    = Noop
    | UrlChanges Navigation.Location
    | UserSelectsPage Page
    | WellsAction Wells.Message
    | PasatAction Pasat.Action



--
-- PORTS
--
port downloadPort : ( String, String, String ) -> Cmd msg



--
-- UPDATE
--


noCmd m =
  ( m, Cmd.none )


urlToPage location =
    case location.hash of
        "#About" -> Just About
        "#Birds" -> Just Birds
        "#Numbers" -> Just Numbers
        _ -> Nothing


pageToUrl page =
    "#" ++ toString page


changePage page oldModel =
    if page == oldModel.page
    then noCmd oldModel
    else
        let
            newModel =
                {oldModel | page = page}
        in
            case oldModel.page of
                Birds -> update (WellsAction Wells.UserStopsAllSounds) newModel
                _ -> noCmd newModel


update : Action -> Model -> ( Model, Cmd Action )
update action oldModel =

  case action of

    UrlChanges location ->
        case urlToPage location of
            Nothing ->
                    (oldModel, Navigation.modifyUrl <| pageToUrl About)

            Just page ->
                changePage page oldModel


    UserSelectsPage page ->
        let
            (newModel, pageCmd) =
                changePage page oldModel

            urlCmd =
                Navigation.newUrl <| pageToUrl page
        in
            ({ newModel | page = page }, Cmd.batch [pageCmd, urlCmd])


    WellsAction wellsAction ->
      let
        ( wellsModel, wellsCmd ) =
          Wells.update wellsAction oldModel.wells
      in
        ( { oldModel | wells = wellsModel }, Cmd.map WellsAction wellsCmd )


    PasatAction pasatAction ->
      let
        ( pasatModel, pasatCmd ) =
          Pasat.update downloadPort pasatAction oldModel.pasat
      in
        ( { oldModel | pasat = pasatModel }, Cmd.map PasatAction pasatCmd )


    Noop ->
      noCmd oldModel



state0 location =
    let
        ( pasatModel0, pasatCmd0 ) =
            Pasat.state0

        ( wellsModel0, wellsCmd0 ) =
            Wells.init

        ( model, urlCmd ) =
            update (UrlChanges location) <| Model About pasatModel0 wellsModel0
    in
        ( model, Cmd.batch [urlCmd, Cmd.map WellsAction wellsCmd0, Cmd.map PasatAction pasatCmd0] )



--
-- VIEW
--
viewWellsDone =
    div
        []
        [ text "Task complete"
        , button [ onClick (UserSelectsPage Numbers) ] [ text "Proceed to next task" ]
        ]


view : Model -> Html.Html Action
view model =
  let
    page =
      case model.page of
        About ->
          AboutView.view

        Birds ->
            div
                []
                [ Html.App.map WellsAction <| Wells.view model.wells
                , if model.wells.currentScript /= Nothing then span [] [] else viewWellsDone
                ]

        Numbers ->
          Html.App.map PasatAction <| PasatView.view model.pasat

    pageSelector page =
        li
            []
            [ button
                [ onClick <| UserSelectsPage page
                , class <| if model.page == page then "nav-selected" else "nav-avaialble"
                ]
                [ text <| toString page ]
            ]
  in
    div
      []
      [ ul [ class "nav" ] <| List.map pageSelector pages
      , page
      ]



--
-- MAIN
--


main =
  Navigation.program
    (Navigation.makeParser identity)
    { init = state0
    , view = view
    , update = update
    , urlUpdate = update << UrlChanges
    , subscriptions = \_ -> Sub.none
    }
