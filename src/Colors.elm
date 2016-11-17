module Colors exposing (init, update, view, subscriptions)

import Basics.Extra exposing ((=>))
import Tuple exposing (first, second)
import Color exposing (Color)
import Response exposing (withCmd, withNone)
import Random.Color exposing (hsl, rgb)
import Random
import Html exposing (Html, Attribute, button, div, text, program)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Color.Convert exposing (colorToCssHsl)


main : Program Never Model Msg
main = program
  { init = init 50
  , update = update 
  , view = view 
  , subscriptions = subscriptions
  }


type alias Fixed = Bool

type alias Index = Int

type alias Model =
  { colors : List (Color, Fixed)
  }


init : Int -> (Model, Cmd Msg)
init length =
  { colors = []
  }
    |> withCmd (newColors length)


newColors : Int -> Cmd Msg
newColors length =
  Random.generate New (Random.list length rgb)


type Msg
  = New (List Color)
  | Roll
  | ToggleFixed Int
  | NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      let
        cmd = List.filter (second >> not) model.colors
          |> List.length
          |> newColors
      in
        withCmd cmd model

    New colors ->
      List.map (\color -> (color, False)) colors
        |> (++) (List.filter second model.colors)
        |> \colors -> { model | colors = colors }
        |> withNone

    ToggleFixed index ->
      let
        tagger : Int -> (Color, Fixed) -> (Color, Fixed)
        tagger idx color =
          if idx == index then
            Tuple.mapSecond not color
          else
            color
      in        
        List.indexedMap tagger model.colors
          |> \colors -> { model | colors = colors }
          |> withNone

    NoOp ->
      model |> withNone


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


view : Model -> Html Msg
view model =
  div
    [ style
      [ "display" => "flex"
      , "flex-wrap" => "wrap"
      , "width" => "500px"
      ]
    ]
    <| rollButton :: viewColors model.colors


rollButton : Html Msg
rollButton =
  button
    [ style
      [ "width" => "50px"
      , "height" => "50px"
      , "margin" => "5px"
      ]
    , onClick Roll
    ]
    [ text "Roll" ]


viewColors : List (Color, Fixed) -> List (Html Msg)
viewColors colors =
  let
    viewColor : Int -> (Color, Fixed) -> Html Msg
    viewColor index (color, fixed) =
      div
        [ style
          [ "background-color" => colorToCssHsl color 
          , "width" => "50px"
          , "height" => "50px"
          , "margin" => "5px"
          , "display" => "flex"
          , "justify-content" => "flex-end"
          , "align-items" => "flex-start"
          ]
        , onClick <| ToggleFixed index
        ]
        [ div
          [ style
            [ "background-color" => "#eee"
            , "width" => "8px"
            , "height" => "8px"
            , "font-size" => "10px"
            , "display" => "flex"
            , "justify-content" => "space-around"
            , "align-items" => "center"
            ]
          ]
          [ text (if fixed then "x" else "")]
        ]
  in
    List.indexedMap viewColor colors
