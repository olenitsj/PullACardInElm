module Main exposing (Model, Msg, cardClosedView, init, subscriptions, update, view)

import Animation exposing (px, scale, rotate, Angle, turn, Step)
import Animation.Messenger exposing (send)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (..)
import Random.List exposing (shuffle)
import Random exposing (..)

main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Card =
    { url : String
    , image : String
    , name : String
    , style : Animation.Messenger.State Msg
    , index : Int
    }


type alias Model =
    { initialCards : List Card
    , randomCards : List Card
    , chosenCardIndex : Maybe Int
    , cardClicked : Bool
    }

cardClosedView : Card -> Html Msg
cardClosedView card =
    img
        [ src "http://www.localhost/wp-content/uploads/2015/11/LOGO-2.jpg"
        , width 200
        , height 200
        , style "z-index" "2006"
        , style "transform" "rotate(12deg)"
        , style "top" "0em"
        , style "border" "3px solid black"
        ]
        []

cardOpenView : Card -> Html Msg
cardOpenView card =
    img
        [ src card.url
        , width 200
        , height 200
        , style "z-index" "3000"
        , style "top" "0em"
        ]
        []

initialWidgetStyle =
            Animation.style [ Animation.left (px 0.0), Animation.opacity 1.0, Animation.paddingTop (px 0), Animation.scale 1.0, Animation.rotate (turn 0.0)]
allCards =
            [ { url = "http://www.localhost/wp-content/uploads/2020/01/music-for-13-chakras-cover-1.jpg"
                , name = "name"
                , image = "image"
                , style = initialWidgetStyle
                , index = 0
                }
            , { url = "http://www.localhost/wp-content/uploads/2017/07/kaart-Kyron-facebook.jpg"
                , name = "name"
                , image = "image"
                , style = initialWidgetStyle
                , index = 1
                }
            , { url = "http://www.localhost/wp-content/uploads/2019/11/remedies.png"
                , name = "name"
                , image = "image"
                , style = initialWidgetStyle
                , index = 2
                },
             { url = "http://www.localhost/wp-content/uploads/2019/11/remedies.png"
                , name = "name"
                , image = "image"
                , style = initialWidgetStyle
                , index = 3
                }
              , { url = "http://www.localhost/wp-content/uploads/2019/11/remedies.png"
                , name = "name"
                , image = "image"
                , style = initialWidgetStyle
                , index = 4
                } 
              , { url = "http://www.localhost/wp-content/uploads/2019/11/remedies.png"
                , name = "name"
                , image = "image"
                , style = initialWidgetStyle
                , index = 5
                }
              , { url = "http://www.localhost/wp-content/uploads/2019/11/remedies.png"
              , name = "name"
              , image = "image"
              , style = initialWidgetStyle
              , index = 6
                }
            ]

getRandomCards cards seed =  Tuple.first (Random.step (shuffle cards) (Random.initialSeed seed))

init : Int -> ( Model, Cmd Msg )
init flag = ( { initialCards = allCards
      , randomCards = getRandomCards allCards flag
      , chosenCardIndex = Nothing
      , cardClicked = False
      }
    , Cmd.none
    )


type Msg
    = ShowCard Int
    | FadeIn Int
    | FadeOut Int
    | Animate Animation.Msg
    | PullAndFlipCard Int

emptyCmd a b = (a b, Cmd.none)

pullAndFlipAnimation i = Animation.interrupt
            [ paddingTop 400 300
            , Animation.Messenger.send (ShowCard i)
            ]
setChosenCardIndex model i =
    { model | chosenCardIndex = Just i }

setCardClickedTrue model =
    { model | cardClicked = True }

openCardAnimation = Animation.interrupt
                    [ Animation.toWith
                        (Animation.speed { perSecond = 400 })
                        [ Animation.scale 3.0
                        ]
                    ]

fadeInAnimation = Animation.interrupt
                    [ paddingTop 300 100
                    ]

paddingTop speed pixels = Animation.toWith
                        (Animation.speed { perSecond = speed })
                        [ Animation.paddingTop (px pixels)
                        ]

fadeOutAnimation = Animation.interrupt
                    [ paddingTop 500 100
                    , paddingTop 500 0
                    ]
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowCard i -> 
            let (a, b) = (onCardStyle model i <| emptyCmd openCardAnimation)
            in (setChosenCardIndex a i, b)
        FadeIn i ->
            ( onCardStyle model i <| emptyCmd fadeInAnimation)
        FadeOut i ->
            ( onCardStyle model i <|
              emptyCmd fadeOutAnimation
            )
        PullAndFlipCard i -> 
           let (a, b) = ( onCardStyle model i <| emptyCmd (pullAndFlipAnimation i))
           in (setCardClickedTrue a, b)
        Animate time ->
            let (a, b) = List.unzip (List.map (onStyle <| Animation.Messenger.update <| time) model.randomCards)
            in ( { model | randomCards = a}
            , Cmd.batch b )

onIndex : Int -> List a -> (a -> (a, Cmd Msg)) -> (List a, List (Cmd Msg))
onIndex i list fn =
    List.unzip (List.indexedMap
        (\j val ->
            if i == j then
                fn val

            else
                (val, Cmd.none)
        )
        list)


onStyle : (Animation.Messenger.State Msg -> (Animation.Messenger.State Msg, Cmd Msg)) -> Card -> (Card, Cmd Msg)
onStyle styleFn card =
    let (newCard, cmd) = styleFn card.style 
    in  ({ card | style = newCard }, cmd)


onCardStyle : Model -> Int -> (Animation.Messenger.State Msg -> (Animation.Messenger.State Msg, Cmd Msg)) -> (Model, Cmd Msg)
onCardStyle model index fn = 
    let (newCard, cmd) = onIndex index model.randomCards <| onStyle fn
    in  ({ model | randomCards = newCard}, Cmd.batch cmd)


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate <| List.map .style model.randomCards


view : Model -> Html Msg
view model =
    ul [ style "text-align" "center", style "margin-top" "80px" ]
        (List.map
            (\card ->
                case model.chosenCardIndex of
                    Just i -> if i == card.index then viewChosenCard cardOpenView card else viewChosenCard cardClosedView card
                    Nothing ->
                        if model.cardClicked then viewChosenCard cardClosedView card else viewCard card
            )
            model.randomCards
        )


viewCard : Card -> Html Msg
viewCard card =
    li
        (basicCardStyle card
            ++ [ onMouseEnter (FadeIn card.index)
               , onMouseLeave (FadeOut card.index)
               , onClick (PullAndFlipCard card.index)
               ]
        )
        [ cardClosedView card ]

basicCardStyle card = Animation.render card.style ++ [ style "display" "inline-block"
               , style "list-style-type" "none"
               , style "cursor" "pointer"
               , style "margin-bottom" "10px"
               , style "margin-left" "-14.5%"
               , style "vertical-align" "top"
               ]



viewChosenCard cardView card =
    li (basicCardStyle card)
        [ cardView card ]
