module NoteFrequencies exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Debug exposing (toString)


initialModel : Model
initialModel =
    { baseFrequency = 440
    }

type alias Model =
    { baseFrequency : Int
    }

type Msg =
    ChangeBaseFrequency Int

frequencies : List Int
frequencies = [ 436, 438, 440, 442, 444 ]

viewFrequencyToggle : Int -> Html Msg
viewFrequencyToggle frequency =
    text (toString frequency)

view : Model -> Html Msg
view model =
    div [ class "content" ]
    [ h1 [] [ text "Note Frequencies" ]
    , div [ class "note-frequencies" ]
        (List.map (viewFrequencyToggle model.baseFrequency) frequencies)
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