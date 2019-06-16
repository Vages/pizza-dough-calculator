module Main exposing (Model, init, main)

import Browser
import Html exposing (..)
import Round exposing (round)


main =
    Browser.sandbox { init = init, view = view, update = update }


update _ model =
    model


view model =
    let
        ingredientAmounts =
            getIngredientAmounts model
    in
    ul []
        [ li [] [ text <| "Flour: " ++ round 1 ingredientAmounts.flour ++ " g" ]
        , li [] [ text <| "Water: " ++ round 1 ingredientAmounts.water ++ " g" ]
        , li [] [ text <| "Salt: " ++ round 1 ingredientAmounts.salt ++ " g" ]
        , li [] [ text <| "Yeast: " ++ round 1 ingredientAmounts.yeast ++ " g" ]
        ]


init =
    { balls_of_dough = 1
    , weight_of_each_ball = 250
    , water_percentage = 57
    }


yeast_as_proportion_of_flour =
    0.002


salt_as_proportion_of_flour =
    0.03


type alias Model =
    { balls_of_dough : Float
    , weight_of_each_ball : Float
    , water_percentage : Float
    }


getIngredientAmounts : Model -> IngredientAmounts
getIngredientAmounts model =
    let
        flour =
            (model.balls_of_dough * model.weight_of_each_ball) / (1 + (model.water_percentage / 100) + yeast_as_proportion_of_flour + salt_as_proportion_of_flour)

        water =
            flour * model.water_percentage / 100

        salt =
            flour * salt_as_proportion_of_flour

        yeast =
            flour * yeast_as_proportion_of_flour
    in
    { flour = flour, water = water, salt = salt, yeast = yeast }


type alias IngredientAmounts =
    { flour : Float
    , water : Float
    , salt : Float
    , yeast : Float
    }
