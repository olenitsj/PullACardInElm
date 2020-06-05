module Main exposing (main)

import Animation exposing (px, turn)
import Animation.Messenger exposing (send)
import Browser
import Html exposing (Html, a, div, img, li, text, ul)
import Html.Attributes exposing (height, href, src, style, width)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Random exposing (initialSeed)
import Random.List exposing (shuffle)


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Card =
    { imageUrl : String
    , productUrl : String
    , style : Animation.Messenger.State Msg
    }


type alias Model =
    { initialCards : List Card
    , randomCards : List Card
    , chosenCard : Maybe String
    , cardClicked : Bool
    }


cardClosedView : Card -> Html Msg
cardClosedView card =
    img
        [ src "http://13grandmothersremedies.com/wp-content/uploads/2015/11/LOGO-600x600.jpg"
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
    div []
        [ div []
            [ img
                [ src card.imageUrl
                , width 600
                , height 600
                , style "z-index" "3000"
                , style "top" "0em"
                ]
                []
            ]
        , div [] [ a [ href card.productUrl, style "font-size" "25px" ] [ text linkText ] ]
        ]


linkText : String
linkText =
    "Klik hier om de bijhorende remedie te bekijken."


initialWidgetStyle : Animation.Messenger.State Msg
initialWidgetStyle =
    Animation.style [ Animation.left (px 0.0), Animation.opacity 1.0, Animation.paddingTop (px 0), Animation.rotate3d (turn 0) (turn 0) (turn 0), Animation.top (px 0) ]


getRandomCards : List Card -> Int -> List Card
getRandomCards cards seed =
    List.take 7 (Tuple.first (Random.step (shuffle cards) (Random.initialSeed seed)))


init : Int -> ( Model, Cmd Msg )
init flag =
    ( { initialCards = allCards
      , randomCards = getRandomCards allCards flag
      , chosenCard = Nothing
      , cardClicked = False
      }
    , Cmd.none
    )


type Msg
    = ShowCard Card
    | FadeIn Card
    | FadeOut Card
    | Animate Animation.Msg
    | PullAndFlipCard Card


emptyCmd a b =
    ( a b, Cmd.none )


pullAndFlipAnimation : Card -> Animation.Messenger.State Msg -> Animation.Messenger.State Msg
pullAndFlipAnimation card =
    Animation.interrupt
        [ paddingTop 200 90
        , paddingTop 200 200
        , Animation.toWith (Animation.speed { perSecond = 6 }) [ Animation.rotate3d (turn -0.25) (turn 0) (turn 0) ]
        , Animation.Messenger.send (ShowCard card)
        ]


setChosenCard : Model -> Card -> Model
setChosenCard model card =
    { model | chosenCard = Just card.imageUrl }


setCardClickedTrue : Model -> Model
setCardClickedTrue model =
    { model | cardClicked = True }


openCardAnimation : Animation.Messenger.State Msg -> Animation.Messenger.State Msg
openCardAnimation =
    Animation.interrupt
        [ Animation.set
            [ Animation.top (px 42)
            ]
        , Animation.toWith
            (Animation.speed { perSecond = 6 })
            [ Animation.rotate3d (turn 0) (turn 0) (turn 0) ]
        ]


fadeInAnimation : Animation.Messenger.State Msg -> Animation.Messenger.State Msg
fadeInAnimation =
    Animation.interrupt
        [ paddingTop 150 42
        ]


paddingTop speed pixels =
    Animation.toWith
        (Animation.speed { perSecond = speed })
        [ Animation.top (px pixels)
        ]


fadeOutAnimation : Animation.Messenger.State Msg -> Animation.Messenger.State Msg
fadeOutAnimation =
    Animation.interrupt
        [ paddingTop 100 42
        , paddingTop 100 0
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowCard card ->
            let
                ( a, b ) =
                    onCardStyle model card <| emptyCmd openCardAnimation
            in
            ( setChosenCard a card, b )

        FadeIn card ->
            onCardStyle model card <| emptyCmd fadeInAnimation

        FadeOut card ->
            onCardStyle model card <|
                emptyCmd fadeOutAnimation

        PullAndFlipCard card ->
            let
                ( a, b ) =
                    onCardStyle model card <| emptyCmd (pullAndFlipAnimation card)
            in
            ( setCardClickedTrue a, b )

        Animate time ->
            let
                ( a, b ) =
                    List.unzip (List.map (onStyle <| Animation.Messenger.update <| time) model.randomCards)
            in
            ( { model | randomCards = a }
            , Cmd.batch b
            )


onIndex : Card -> List Card -> (Card -> ( Card, Cmd Msg )) -> ( List Card, List (Cmd Msg) )
onIndex card list fn =
    List.unzip
        (List.map
            (\val ->
                if card.imageUrl == val.imageUrl then
                    fn val

                else
                    ( val, Cmd.none )
            )
            list
        )


onStyle : (Animation.Messenger.State Msg -> ( Animation.Messenger.State Msg, Cmd Msg )) -> Card -> ( Card, Cmd Msg )
onStyle styleFn card =
    let
        ( newCard, cmd ) =
            styleFn card.style
    in
    ( { card | style = newCard }, cmd )


onCardStyle : Model -> Card -> (Animation.Messenger.State Msg -> ( Animation.Messenger.State Msg, Cmd Msg )) -> ( Model, Cmd Msg )
onCardStyle model card fn =
    let
        ( newCard, cmd ) =
            onIndex card model.randomCards <| onStyle fn
    in
    ( { model | randomCards = newCard }, Cmd.batch cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate <| List.map .style model.randomCards


view : Model -> Html Msg
view model =
    ul [ style "text-align" "center", style "min-height" "100%", style "max-width" "940px", style "margin-right" "auto", style "margin-bottom" "auto", style "margin-left" "auto", style "min-height" "25em" ]
        (List.map
            (\card ->
                case model.chosenCard of
                    Just url ->
                        if url == card.imageUrl then
                            viewChosenCard cardOpenView card

                        else
                            Html.text ""

                    Nothing ->
                        if model.cardClicked then
                            viewChosenCard cardClosedView card

                        else
                            viewCard card
            )
            model.randomCards
        )


viewCard : Card -> Html Msg
viewCard card =
    li
        (basicCardStyle card
            ++ [ onMouseEnter (FadeIn card)
               , onMouseLeave (FadeOut card)
               , onClick (PullAndFlipCard card)
               ]
        )
        [ cardClosedView card ]


basicCardStyle : Card -> List (Html.Attribute msgB)
basicCardStyle card =
    Animation.render card.style
        ++ [ style "display" "inline-block"
           , style "list-style-type" "none"
           , style "cursor" "pointer"
           , style "margin-bottom" "10px"
           , style "margin-left" "-14.5%"
           , style "vertical-align" "top"
           , style "position" "relative"
           , style "max-width" "100%"
           ]


viewChosenCard cardView card =
    li (basicCardStyle card)
        [ cardView card ]


allCards : List Card
allCards =
    grootmoederCards ++ grootvaderCards ++ fotonengordelCards ++ aanvullendeRemediesCards ++ nieuweTijdRemedies ++ chakrasremedies


grootmoederCards : List Card
grootmoederCards =
    [ { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GM1.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootmoederremedies/kaarten-gm1/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GM2.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootmoederremedies/kaarten-gm2/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GM3.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootmoederremedies/kaarten-gm3/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GM4.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootmoederremedies/kaarten-gm4/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GM5.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootmoederremedies/kaarten-gm5/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GM6.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootmoederremedies/kaarten-gm6/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GM7.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootmoederremedies/kaarten-gm7/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GM8.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootmoederremedies/kaarten-gm8/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GM9.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootmoederremedies/kaarten-gm9/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GM10.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootmoederremedies/kaarten-gm10/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GM11.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootmoederremedies/kaarten-gm11/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GM12.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootmoederremedies/kaarten-gm12/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GM13.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootmoederremedies/kaarten-gm13/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GMF.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootmoederremedies/kaarten-gmf/"
      , style = initialWidgetStyle
      }
    ]


grootvaderCards : List Card
grootvaderCards =
    [ { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GF1.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootvaderremedies/kaarten-gff/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GF2.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootvaderremedies/kaarten-gff/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GF3.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootvaderremedies/kaarten-gff/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GF4.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootvaderremedies/kaarten-gf5/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GF5.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootvaderremedies/kaarten-gf5/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GF6.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootvaderremedies/kaarten-gf6/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GF7.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootvaderremedies/kaarten-gf7/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GFF.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/de-grootvaderremedies/kaarten-gff/"
      , style = initialWidgetStyle
      }
    ]


aanvullendeRemediesCards : List Card
aanvullendeRemediesCards =
    [ { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-Agnihotra.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/aanvullende-remedies/kaarten-agnihotra/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-levensbloem.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/aanvullende-remedies/kaarten-levensbloem/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/KaartenGOV.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/aanvullende-remedies/kaartengov/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/the-One.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/aanvullende-remedies/the-one/"
      , style = initialWidgetStyle
      }
    ]


fotonengordelCards : List Card
fotonengordelCards =
    [ { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/Kaarten-GF-Fotonengordel-web.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/fotonengordelremedie/kaarten-gf-fotonengordel-web/"
      , style = initialWidgetStyle
      }
    ]


nieuweTijdRemedies : List Card
nieuweTijdRemedies =
    [ { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2017/03/Annunaki.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/nieuwe-tijd-remedies/annunaki/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2017/03/Ashtar.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/nieuwe-tijd-remedies/ashtar/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2017/03/Fusie-van-de-Zwarte-Gaten-.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/nieuwe-tijd-remedies/de-fusie-van-de-zwarte-gaten/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2019/11/Kaart-hoeders-van-de-aarde-E1-pdf.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/nieuwe-tijd-remedies/de-hoeders-van-de-aarde/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2017/03/Gor.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/nieuwe-tijd-remedies/gor/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2017/03/Ho%E2%80%99Oponopono-.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/nieuwe-tijd-remedies/hooponopono/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2017/07/kaart-Kyron-facebook.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/nieuwe-tijd-remedies/kryon/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2017/03/Merlijn.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/nieuwe-tijd-remedies/merlijn/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2017/03/Passie8.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/nieuwe-tijd-remedies/passie/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2017/03/godsvonk.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/nieuwe-tijd-remedies/verbinding-met-je-godsvonk/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2017/03/Yrtl.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/nieuwe-tijd-remedies/yrtl/"
      , style = initialWidgetStyle
      }
    ]


chakrasremedies : List Card
chakrasremedies =
    [ { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2018/04/Kaarten-Chakra1.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/13-chakras-remedies/chakra-1stuitchakra-of-basischakra/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2018/04/Kaarten-Chakra2.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/13-chakras-remedies/chakra-2-heiligbeen-of-sacraalchakra/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2018/04/Kaarten-Chakra3.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/13-chakras-remedies/chakra-3-zonnevlechtchakra/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2018/04/Kaarten-Chakra4.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/13-chakras-remedies/chakra4-milt-of-middenrifchakra/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2018/04/Kaarten-Chakra5.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/13-chakras-remedies/chakra-5-hartchakra/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2018/04/Kaarten-Chakra6.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/13-chakras-remedies/chakra-6-thymuschakra/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2018/04/Kaarten-Chakra7.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/13-chakras-remedies/chakra-7-keelchakra/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2018/04/Kaarten-Chakra8.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/13-chakras-remedies/chakra-8-kosmische-doorstromingschakra-of-droomchakra/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2018/04/Kaarten-Chakra9.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/13-chakras-remedies/chakra-9-hypofysechakra/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2018/04/Kaarten-Chakra10.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/13-chakras-remedies/chakra-10-epifysechakra/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2018/04/Kaarten-Chakra11.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/13-chakras-remedies/chakra-11-kruinchakra/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2018/04/Kaarten-Chakra12.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/13-chakras-remedies/chakra-12-transformatiechakra-of-de-kosmische-vrouw/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2018/04/Kaarten-Chakra13.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/13-chakras-remedies/chakra-13-transmutatiechakra-of-de-kosmische-man/"
      , style = initialWidgetStyle
      }
    , { imageUrl = "https://www.13grandmothersremedies.com/wp-content/uploads/2018/04/Kaarten-Chakra-balans.jpg"
      , productUrl = "https://www.13grandmothersremedies.com/producten/13-chakras-remedies/chakrabalans/"
      , style = initialWidgetStyle
      }
    ]
