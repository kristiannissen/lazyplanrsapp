module LazyPlanrsApp exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Date exposing (Date)
import Result
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

type alias Swimlane =
    { name : String
    , objectives : List Objective
    }

type alias Model =
    { swimlanes : List Swimlane
    , now_date : Date
    }

-- Update

type Msg =
    AddSwimlane
    | Sync

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg |> log "Msg" of
        AddSwimlane ->
            ( { model | swimlanes = model.swimlanes ++ [{name = "Bogus", objectives = [] }] }, Cmd.none )

        Sync ->
            ( model, Cmd.none )
-- View

view : Model -> Html Msg
view model =
    div [] [
        div [ class "tools", id "toolbar" ][
            span [][ text (toString model.now_date)]
            , button [ onClick AddSwimlane ][ text "Add Swimlane" ]
            , button [ onClick Sync ][ text "Sync" ]
        ]
        , div [] [ swimlaneList model.swimlanes ]
        , div [][
            code [][ text (toString model) ]
        ]
    ]

swimlaneList : List Swimlane -> Html Msg
swimlaneList swimlanes =
    div [][
        div [] (List.map (swimlaneItem) swimlanes)
    ]

swimlaneItem : Swimlane -> Html Msg
swimlaneItem swimlane =
    div [ class "swimlane" ][
        div [][
            button [][ text "Edit Name" ]
        ]
        , div [][ text swimlane.name ]
        , div [ class "objectives" ][ objectiveList swimlane.objectives ]
    ]

objectiveList : List Objective -> Html Msg
objectiveList objectives =
    div [] (List.map (objectiveItem) objectives)

objectiveItem : Objective -> Html Msg
objectiveItem objective =
    div [ class "objective" ] [
        div [][
            button [][ text "Edit Name"]
        ]
        , div [] [ text objective.name ]
        , div [ class "initiatives" ] [ initiativeList objective.initiatives ]
    ]

initiativeList : List Initiative -> Html Msg
initiativeList initiatives =
    div [ class "initiative" ] (List.map (initiativeItem) initiatives)

initiativeItem : Initiative -> Html Msg
initiativeItem initiative =
    div [][
        div [][
            button [][ text "Edit Initiative" ]
        ]
        , div [][ text initiative.name ]
        , div [][ text (toString initiative.startdate) ]
        , div [][ text (toString initiative.duration) ]
        , div [][ text (toString initiative.business_value) ]
        , div [][ text (toString initiative.developer_effort) ]
        , div [][ text (toString initiative.owner) ]
        , div [][ text (toString (List.length initiative.features)) ]
    ]

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- Init

objectives : List Objective
objectives =
    [({ name = "Do something", initiatives = [
        ({ name = "Think of something", startdate = parseDate "2016-01-10",
            duration = Just 5, business_value = Nothing, developer_effort = Nothing,
            owner = Just "kn@unisport.dk", features = [({ name = "Green Button", description = Nothing})] })
        , ({ name = "Define the idea", startdate = parseDate "2016-10-10",
            duration = Nothing, business_value = Just 5, developer_effort = Just 8,
            owner = Just "kn@unisport.dk", features = [] })
        ] })
    , ({ name = "Do something else", initiatives = [] })
    ]

swimlanes : List Swimlane
swimlanes =
    [{ name = "IT", objectives = objectives }]

init : ( Model, Cmd Msg )
init =
    ( { swimlanes = swimlanes, now_date = (Date.fromTime Time.millisecond) }, Cmd.none )

-- Main

main =
    Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
