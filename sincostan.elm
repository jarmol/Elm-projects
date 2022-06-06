module Main exposing (..)

-- Press buttons to increment and decrement angle by+/- 1 or +/- 5 degrees.
--
-- This program is extended from:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, button, div, h1, p, text, sup, b)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import FormatNumber exposing (..)
import FormatNumber.Locales exposing (..)

-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model = Int

init : Model
init =
  45


-- UPDATE

type Msg
  = Increment
  | Increm5
  | Decrement
  | Decrem5


update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Increm5   ->
      model + 5

    Decrement ->
      model - 1

    Decrem5   ->
      model - 5


toRad : Int -> Float
toRad angleDegrees =
  pi * (toFloat angleDegrees) / 180

dform nd x =
        format { frenchLocale | decimals = Exact nd } x

sx6 sx =
   sx -  (sx^ 3) / 6
   + (sx^ 5) / 120
   - (sx^ 7) / 5040
   + (sx^ 9) / 362880
   - (sx^ 11) / 39916800

cx6 cx =
   1 - (cx^ 2) / 2
   + (cx^ 4) / 24
   - (cx^ 6) / 720
   + (cx^ 8) / 40320
   - (cx^ 10) / 3628800
   + (cx^ 12) / 479001600
   


-- VIEW

view : Model -> Html Msg
view model =
  let xi = toRad model
      lab6 = "sin x ≅ x - x³ / 3! + x⁵ / 5! - x⁷ / 7! + x⁹ / 9! - x¹¹ / 11! = "
      labc = "cos x ≅ 1 - x² / 2! + x⁴ / 4! - x⁶ / 6! + x⁸ / 8! - x"
      supStyle = [style "font-size" "65%", style "font-weight" "bold"]
      labe =   " /10! + x¹² /12! = "
  in
  div [style "margin" "5%", style "font-size" "125%"]
    [ h1 [style "font-family" "Helvetica Neue",
      style "color" "red"][text "Power series of SIN,  COS and TAN"] 
    , button [ onClick Increment ] [ text "+1" ]
    , button [ onClick Increm5 ] [ text "+5" ]
    , div [] [ text (String.fromInt model ++ "° angle") ]
    , button [ onClick Decrement ] [ text "-1" ]
    , button [ onClick Decrem5 ] [ text "-5" ]
    , p [][text ("sin " ++ String.fromInt model ++ "° = "
        ++ dform 12 (sin xi))]
    , p [][text ("cos " ++ String.fromInt model ++ "° = "
        ++ dform 12 (cos xi))]
    , p [][text ("tan " ++ String.fromInt model ++ "° = "
        ++ dform 12 (tan xi))]
    , p [] [text ("x = " ++String.fromInt model
     ++ "° = " ++ dform 6 xi ++ " radians")]
    , p [] [text (lab6 ++ dform 12 (sx6 xi))]
    , p [] [text (labc)
--  , sup [style "font-size" "65%", style "font-weight" "bold"][text "10"]
    , sup supStyle [text "10"]
    , text (labe ++ dform 12 (cx6 xi))]
    , p [][]
    , text ("tan x = sin x / cos x ≅ " ++ dform 10 ((sx6 xi)/(cx6 xi)))
    ]
