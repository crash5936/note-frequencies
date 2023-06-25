module NoteFrequencies exposing (main)

import Browser
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Round

initialModel : Model
initialModel =
    { baseFrequency = 440
    }


type alias Model =
    { baseFrequency : Int
    }


type Msg
    = ChangeBaseFrequency Int


frequencies : List Int
frequencies =
    [ 436, 438, 440, 442, 444 ]


viewFrequencyToggle : Int -> Html Msg
viewFrequencyToggle frequency =
    li [ onClick (ChangeBaseFrequency frequency) ] [ text (toString frequency) ]


noteRow : String -> Int -> Html Msg
noteRow note frequency =
    tr []
        [ td [] [ text note ]
        , td [] [ text (toString frequency ++ " Hz") ]
        ]


calculateFrequency : Int -> Int -> Float
calculateFrequency halftones baseFrequency =
    let
        constant = 2 ^ (1/12) -- the twelfth root of 2, because an octave has 12 half-steps and a tone doubles in frequency every octave in an equal tempered scale
    in
    (toFloat baseFrequency * (constant ^ toFloat halftones))


tones : Int -> Html Msg
tones baseFrequency =
    let
        lowerThanBase =
            List.map (noteRow "l") (List.range -47 -1)

        higherThanBase =
            List.map (noteRow "h") (List.range 1 47)
    in
    table [ id "frequencies" ]
        ([ tr []
            [ th [] [ text "Note" ]
            , th [] [ text "Frequency" ]
            ]
         ]
            ++ lowerThanBase
            ++ [ noteRow "A₄" baseFrequency ]
            ++ higherThanBase
        )


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Note Frequencies" ]
        , div [ class "note-frequencies" ] []
        , ul [ id "frequency-toggle" ]
            (List.map viewFrequencyToggle frequencies)
        , tones model.baseFrequency
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeBaseFrequency newBaseFrequency ->
            { model | baseFrequency = newBaseFrequency }


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
