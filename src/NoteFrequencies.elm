module NoteFrequencies exposing (main)

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


octaveNotes : List String
octaveNotes =
    [ "A", "A♯", "B", "C", "C♯", "D", "D♯", "E", "F", "F♯", "G", "G♯" ]


reverseOctaveNotes : List String
reverseOctaveNotes =
    [ "A", "G♯", "G", "F♯", "F", "E", "D♯", "D", "C♯", "C", "B", "A♯" ]


subNumber : Int -> String
subNumber number =
    let
        subNums = 
            Array.fromList [ "₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉" ]
        maybeSubNum =
            Array.get number subNums
        subNum =
            case maybeSubNum of
                Just num ->
                    num
                Nothing ->
                    "₀₀₀"
    in
        subNum
 


getNote : Int -> String
getNote halfsteps =
    let
        notes =
            if halfsteps < 0 then
                Array.fromList reverseOctaveNotes

            else
                Array.fromList octaveNotes

        index =
            modBy 12 (abs halfsteps)

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

getOctave : Int -> Int
getOctave halfsteps =
    let
        baseFreqOctave = 4
        -- integer division by 12, ie. number of halfsteps in an octave
        octaveDistance = halfsteps // 12 
        octave = 4 + octaveDistance
    in
    octave
    




viewFrequencyToggle : Int -> Html Msg
viewFrequencyToggle frequency =
    li [ onClick (ChangeBaseFrequency frequency) ] [ text (toString frequency) ]


noteRow : String -> Int -> Float -> Html Msg
noteRow note octave frequency =
    tr []
        [ td [] [ text (note ++ subNumber octave) ]
        , td [] [ text (Round.round 2 frequency ++ " Hz") ]
        ]


calculateFrequency : Int -> Int -> Float
calculateFrequency baseFrequency halftones =
    let
        -- the twelfth root of 2, because an octave has 12 half-steps and a tone doubles in frequency every octave in an equal tempered scale
        constant =
            2 ^ (1 / 12)

        
    in
    toFloat baseFrequency * (constant ^ toFloat halftones)


tones : Int -> Html Msg
tones baseFrequency =
    let
        lowerThanBaseOctaves =
            List.range -47 -1
                |> List.map getOctave

        higherThanBaseOctaves =
            List.range 1 47
                |> List.map getOctave

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
            List.map3 noteRow lowerThanBaseNotes lowerThanBaseOctaves lowerThanBaseFreq

        higherThanBase =
            List.map3 noteRow higherThanBaseNotes higherThanBaseOctaves higherThanBaseFreq
    in
    table [ id "frequencies" ]
        ([ tr []
            [ th [] [ text "Note" ]
            , th [] [ text "Frequency" ]
            ]
         ]
            ++ lowerThanBase
            ++ [ noteRow "A" 4 (toFloat baseFrequency) ]
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
