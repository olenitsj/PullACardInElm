module Main exposing(..)

import Models exposing(..)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Tuple exposing (..)



main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }

model : Model
model =  Models.initialModel

type Msg = ShowCard

update : Msg -> Model -> Model
update msg model = 
  case msg of
    ShowCard ->
      model

view : List ( a, b ) -> Html Msg
view model =
  div [] (displayCards 5)

displayCard : Html Msg
displayCard = (div[ onClick ShowCard ][text ("This is a Card")])


displayCards : comparable -> List (Html Msg)
displayCards n = if n > 0 then 
  displayCards (n-1) else []