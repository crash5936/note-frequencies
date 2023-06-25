module NoteFrequencies exposing (main)

import Array
import Array exposing (Array)
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

octave : List String
octave = 
    [ "A", "A♯", "B", "C", "C♯", "D", "D♯", "E", "F", "F♯", "G", "G♯" ]

subNumber : Int -> String
subNumber number =
    "0"


getNote : Int -> String
getNote halfsteps =
    let
        notes = 
            if halfsteps < 0
            then Array.fromList (List.reverse octave)
            else Array.fromList octave
        index = modBy 12 (abs halfsteps)
        maybeNote =
            Array.get index notes
        note =
            case maybeNote of
                Just noteStr ->
                    noteStr
                Nothing ->
                    "err"
    in
        note

viewFrequencyToggle : Int -> Html Msg
viewFrequencyToggle frequency =
    li [ onClick (ChangeBaseFrequency frequency) ] [ text (toString frequency) ]


noteRow : String -> Float -> Html Msg
noteRow note frequency =
    tr []
        [ td [] [ text note ]
        , td [] [ text (Round.round 2 frequency ++ " Hz") ]
        ]


calculateFrequency : Int -> Int -> Float
calculateFrequency baseFrequency halftones =
    let
        constant = 2 ^ (1/12) -- the twelfth root of 2, because an octave has 12 half-steps and a tone doubles in frequency every octave in an equal tempered scale
    in
    (toFloat baseFrequency * (constant ^ toFloat halftones))


tones : Int -> Html Msg
tones baseFrequency =
    let
        lowerThanBaseFreq =
            List.range -47 -1
            |> List.map (calculateFrequency baseFrequency)

        higherThanBaseFreq =
            List.range 1 47
            |> List.map (calculateFrequency baseFrequency)

        lowerThanBaseNotes =
            List.range -47 -1
            |> List.map getNote

        higherThanBaseNotes =
            List.range 1 47
            |> List.map getNote

        lowerThanBase =
            List.map2 noteRow lowerThanBaseNotes lowerThanBaseFreq

        higherThanBase =
            List.map2 noteRow higherThanBaseNotes higherThanBaseFreq

        -- lowerThanBase =
        --     List.map (noteRow "l") (List.range -47 -1)

        -- higherThanBase =
        --     List.map (noteRow "h") (List.range 1 47)
    in
    table [ id "frequencies" ]
        ([ tr []
            [ th [] [ text "Note" ]
            , th [] [ text "Frequency" ]
            ]
         ]
            ++ lowerThanBase
            ++ [ noteRow "A₄" (toFloat baseFrequency) ]
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
