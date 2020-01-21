module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Die =
    { face : List Int
    , state : DieState
    }


type alias Model =
    { die1 : Die
    , die2 : Die
    }


type DieState
    = Bouncing
    | Final


initialModel : Model
initialModel =
    { die1 = { face = [ 1 ], state = Bouncing }
    , die2 = { face = [ 1 ], state = Bouncing }
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Random.generate NewFaces
        (bounceCounts
            |> Random.andThen (\( a, b ) -> generateFaces a b)
        )
    )



--UPDATE


type Msg
    = NoOp
    | Roll
    | NewFaces ( List Int, List Int )
    | NewBounce


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( { model
                | die1 = updateDieState Bouncing model.die1
                , die2 = updateDieState Bouncing model.die2
              }
            , Random.generate NewFaces
                (bounceCounts
                    |> Random.andThen (\( a, b ) -> generateFaces a b)
                )
            )

        NewFaces ( newFace1, newFace2 ) ->
            let
                newDie1 =
                    updateDieFace newFace1 model.die1

                newDie2 =
                    updateDieFace newFace2 model.die2
            in
            ( { model | die1 = newDie1, die2 = newDie2 }
            , Cmd.none
            )

        NewBounce ->
            let
                die1 =
                    model.die1

                die2 =
                    model.die2

                newDie1 =
                    case model.die1.state of
                        Bouncing ->
                            case die1.face of
                                currentFace :: [] ->
                                    updateDieState Final die1

                                currentFace :: rest ->
                                    updateDieFace rest die1

                                [] ->
                                    die1

                        Final ->
                            die1

                newDie2 =
                    case model.die2.state of
                        Bouncing ->
                            case die2.face of
                                currentFace :: [] ->
                                    updateDieState Final die2

                                currentFace :: rest ->
                                    updateDieFace rest die2

                                [] ->
                                    die2

                        Final ->
                            die2
            in
            ( { model | die1 = newDie1, die2 = newDie2 }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


dieRoll : Random.Generator Int
dieRoll =
    Random.int 1 6


bounceCount : Random.Generator Int
bounceCount =
    Random.int 5 10


bounceCounts : Random.Generator ( Int, Int )
bounceCounts =
    Random.pair bounceCount bounceCount


generateFaces : Int -> Int -> Random.Generator ( List Int, List Int )
generateFaces count1 count2 =
    Random.pair (Random.list count1 dieRoll) (Random.list count2 dieRoll)


updateDieFace : List Int -> Die -> Die
updateDieFace newFaces die =
    { die | face = newFaces }


updateDieState : DieState -> Die -> Die
updateDieState newState die =
    { die | state = newState }



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { die1, die2 } =
    case ( die1.state, die2.state ) of
        ( Final, Final ) ->
            Sub.none

        ( _, _ ) ->
            Time.every 250 (\_ -> NewBounce)



-- VIEW


view : Model -> Html Msg
view model =
    let
        currentFace1 =
            List.head model.die1.face
                |> Maybe.withDefault 1

        currentFace2 =
            List.head model.die2.face
                |> Maybe.withDefault 1
    in
    div []
        [ h1 [] [ text (String.fromInt currentFace1) ]
        , h1 [] [ text (String.fromInt currentFace2) ]
        , button [ onClick Roll ] [ text "Roll Again" ]
        ]
