import Html.App as Html
import Html exposing (..)
import Svg as Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Window exposing (..)

import Date exposing (Date)
import Result
import Task
import Time

import Debug exposing (log)

-- Utils

parseDate : String -> Maybe Date
parseDate str =
    Result.toMaybe (Date.fromString str)

-- Models

type alias Feature =
    { name : String
    , description : Maybe String
    }

type alias Initiative =
    { name : String
    , startdate : Maybe Date
    , duration : Maybe Int
    , business_value : Maybe Int
    , developer_effort : Maybe Int
    , owner : Maybe String
    , features : List Feature
    }

type alias Objective =
    { name : String
    , initiatives : List Initiative
    }

type alias Model =
    { objectives : List Objective
    , now_date : Date
    , window_size : Window.Size
    }

-- Update

type Msg =
    AddObjective
    | Sync
    | Resize Window.Size
    | Fail

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg |> log "Msg" of
        AddObjective ->
            ( { model | objectives = model.objectives ++ [{name = "Bogus", initiatives = [] }] }, Cmd.none )

        Sync ->
            ( model, Cmd.none )

        Resize size ->
            ( model, Cmd.none )

        Fail ->
            ( model, Cmd.none )

-- View

view : Model -> Html Msg
view model =
    div [] [
        div [ class "tools", id "toolbar" ][
            span [][ Html.text (toString model.now_date)]
            , button [ onClick AddObjective ][ Html.text "Add Objective" ]
            , button [ onClick Sync ][ Html.text "Sync" ]
        ]
        , Svg.svg [ Svg.Attributes.width "100%", Svg.Attributes.height <| toString <| List.length model.objectives * 25 + 50 ][
            backgroundRow model.objectives
            , barList model.objectives
            , legendRow model.objectives
        ]
        , aside [][
            code [][ Html.text (toString model) ]
            , pre [][ Html.text (toString Window.Size) ]
        ]
    ]

barList : List Objective -> Svg Msg
barList list =
    Svg.g [] (List.indexedMap barItem list)


barItem : Int -> Objective -> Svg Msg
barItem ix item =
    Svg.rect [ x "320", Svg.Attributes.width "100", Svg.Attributes.height "25"
                , rx "5", ry "5", fill "#c6dafc", y <| toString <| (ix + 1) * 30 ][]


backgroundRow : List Objective -> Svg Msg
backgroundRow list =
    Svg.g[] (List.indexedMap backgroundCell list)


backgroundCell : Int -> Objective -> Svg Msg
backgroundCell ix item =
    Svg.g [][
        Svg.rect [ Svg.Attributes.width "100%", x "0", fill "#efefef", y <| toString <| (ix + 1) * 30 ][]
        , Svg.line [ x1 "0", x2 "100%", fill "none", stroke "#0e0e0e", y1 <| toString <| (ix + 1) * 30
            ,y2 <| toString <| (ix + 1) * 30 ][]
    ]


legendRow : List Objective -> Svg Msg
legendRow list =
    Svg.g [] (List.indexedMap legendCell list)


legendCell : Int -> Objective -> Svg Msg
legendCell ix item =
    Svg.text' [ x "10", y <| toString <| (ix + 1) * 25 ][ Svg.text item.name ]


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Window.resizes Resize ]

-- Init

-- Data seed

objectives : List Objective
objectives =
    [({ name = "Do something", initiatives = [
        ({ name = "Think of something", startdate = parseDate "2016-01-10",
            duration = Just 5, business_value = Nothing, developer_effort = Nothing,
            owner = Just "kn@unisport.dk", features = [({ name = "Green Button", description = Nothing})] })
        , ({ name = "Define the idea", startdate = parseDate "2016-10-10",
            duration = Nothing, business_value = Just 5, developer_effort = Just 8,
            owner = Just "kn@unisport.dk", features = [{ name = "Blue button", description = Just "I can click it" }] })
        ] })
    , ({ name = "Do something else", initiatives = [] })
    ]


init : ( Model, Cmd Msg )
init =
    ( { objectives = objectives
        , now_date = (Date.fromTime Time.millisecond)
        , window_size = (Window.Size 0 0)}
    , Task.perform (\_ -> Fail) (\x -> Resize x) Window.size
    )

-- Main

main =
    Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
