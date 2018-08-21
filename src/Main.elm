module Main
{-| 
-}
import Css exposing (..)
import Models exposing(..)
import Html.Styled exposing (Html, button, div, text, img)
import Html.Styled.Attributes as Attr exposing (css, src)



main : Program Never Model Msg
main =
  Html.Styled.beginnerProgram { model = model, view = view, update = update }

model : Model
model =  Models.initialModel

type Msg = ShowCard

update : Msg -> Model -> Model
update msg model = 
  case msg of
    ShowCard ->
      model

view : Model -> Html.Styled.Html Msg
view model =
  Html.Styled.div[Attr.class "wrapper"][
    Html.Styled.div [Attr.class "cards", css[]] (cards 7),
    Html.Styled.div [Attr.class "linkToProduct"] []
  ]

card : Html msg
card = (Html.Styled.div[Attr.class "card"][Html.Styled.img[src "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/LOGO-600x600.jpg",
            css
            [ display inlineBlock
            , marginLeft(pct -5)
            , float right
            , transform (rotate (deg 7))
            , padding (px 0)
            , border3 (px 3) solid (rgb 0 0 0)
            , zIndex (int 2000)
            , hover
                [ borderColor theme.primary
                , borderRadius (px 10)
                ]
            , height (px 200)
            , width (px 200)
            ]][]])

cards num =
  if num > 1 then
    card::cards (num-1)
  else 
    card::[]

theme : { secondary : Color, primary : Color }
theme =
    { primary = hex "55af6a"
    , secondary = rgb 250 240 230
    }