module Main exposing (Model, init, main)

import Browser
import Element exposing (Color, rgb, shrink)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input
import Element.Region
import Html exposing (Html)
import Round exposing (round)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, view = view, update = update }


type alias Model =
    { balls_of_dough : Int
    , weight_of_each_ball : Float
    , water_percentage : Float
    }


init : Model
init =
    { balls_of_dough = 1
    , weight_of_each_ball = 250
    , water_percentage = 57
    }


type Msg
    = WaterPercentageChanged Float
    | DoughBallWeightChanged Float
    | SetNumberOfDoughBalls Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        WaterPercentageChanged x ->
            { model | water_percentage = x }

        DoughBallWeightChanged x ->
            { model | weight_of_each_ball = x }

        SetNumberOfDoughBalls x ->
            { model | balls_of_dough = x }


view : Model -> Html Msg
view model =
    let
        ingredientAmounts =
            getIngredientAmounts model

        ingredientsAsList =
            [ { name = "flour", amount = ingredientAmounts.flour }
            , { name = "water", amount = ingredientAmounts.water }
            , { name = "yeast", amount = ingredientAmounts.yeast }
            , { name = "salt", amount = ingredientAmounts.salt }
            ]

        parseDoughBalls newValue =
            if newValue == "" then
                SetNumberOfDoughBalls 0

            else
                let
                    parseResult =
                        String.toInt newValue
                in
                SetNumberOfDoughBalls <| Maybe.withDefault model.balls_of_dough parseResult

        numberOfDoughBallsInput =
            Element.Input.text
                []
                { onChange = parseDoughBalls
                , text = String.fromInt model.balls_of_dough
                , placeholder = Nothing
                , label = Element.Input.labelAbove [] (Element.text "Number of dough balls")
                }

        doughBallInput =
            Element.Input.slider
                [ Element.behindContent
                    (Element.el
                        [ Element.width Element.fill
                        , Element.height (Element.px 2)
                        , Element.centerY
                        , Background.color (rgb 0 0.5 0)
                        , Border.rounded 2
                        ]
                        Element.none
                    )
                ]
                { onChange = DoughBallWeightChanged
                , label = Element.Input.labelAbove [] (Element.text ("Dough ball weight: " ++ round 0 model.weight_of_each_ball))
                , min = 200
                , max = 300
                , value = model.weight_of_each_ball
                , thumb = Element.Input.defaultThumb
                , step = Just 10
                }

        waterPercentageInput =
            Element.Input.slider
                [ Element.behindContent
                    (Element.el
                        [ Element.width Element.fill
                        , Element.height (Element.px 2)
                        , Element.centerY
                        , Background.color (rgb 0 0.5 0)
                        , Border.rounded 2
                        ]
                        Element.none
                    )
                ]
                { onChange = WaterPercentageChanged
                , label = Element.Input.labelAbove [] (Element.text ("Water percentage: " ++ round 0 model.water_percentage))
                , min = 50
                , max = 70
                , value = model.water_percentage
                , thumb = Element.Input.defaultThumb
                , step = Just 1
                }

        ingredientTable =
            Element.table []
                { data = ingredientsAsList
                , columns =
                    let
                        tablePadding =
                            Element.padding 10
                    in
                    [ { header = Element.text "Ingredient" |> Element.el [ Font.bold, tablePadding ]
                      , width = shrink
                      , view = \ingredient -> ingredient.name |> Element.text |> Element.el [ tablePadding ]
                      }
                    , { header = Element.text "Grams" |> Element.el [ Font.bold, tablePadding ]
                      , width = shrink
                      , view = \ingredient -> ingredient.amount |> round 1 |> Element.text |> Element.el [ tablePadding ]
                      }
                    ]
                }
    in
    Element.layout [] <|
        Element.column [ Element.centerX, Element.centerY ]
            [ Element.row
                [ Element.Region.heading 1
                , Font.bold
                , Font.center
                , Font.size 32
                ]
                [ Element.text "Pizza dough calculator" ]
            , numberOfDoughBallsInput
            , doughBallInput
            , waterPercentageInput
            , ingredientTable
            ]


type alias IngredientAmounts =
    { flour : Float
    , water : Float
    , salt : Float
    , yeast : Float
    }


getIngredientAmounts : Model -> IngredientAmounts
getIngredientAmounts model =
    let
        flour =
            (toFloat model.balls_of_dough * model.weight_of_each_ball) / (1 + (model.water_percentage / 100) + yeast_as_proportion_of_flour + salt_as_proportion_of_flour)

        water =
            flour * model.water_percentage / 100

        salt =
            flour * salt_as_proportion_of_flour

        yeast =
            flour * yeast_as_proportion_of_flour

        yeast_as_proportion_of_flour =
            0.002

        salt_as_proportion_of_flour =
            0.03
    in
    { flour = flour, water = water, salt = salt, yeast = yeast }
