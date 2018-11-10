module Main exposing (main)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import UrlParser exposing (..)



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- FLAGS


type alias Flags =
    ()



-- MODEL


type alias Model =
    { value : String }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { value = "" }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Input String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input s ->
            ( { model | value = s }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput Input, size 100 ] []
        , div []
            [ text
                (testParse
                    { path = [ "foo", "1" ]
                    , query = Dict.fromList [ ( "q", "evan" ) ]
                    }
                )
            ]
        ]


testParse : Url -> String
testParse url =
    parse parser url
        |> Debug.toString



-- EXAMPLES


parser : UrlParser ( Int, Maybe String )
parser =
    UrlParser.succeed Tuple.pair
        |> andIgnore (UrlParser.s "foo")
        |> andMap UrlParser.int
        |> andMap (UrlParser.query "q" identity)
