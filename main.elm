module Main exposing (..)

import Array exposing (Array)
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Message =
  { operation: String, data: String }


type alias Photo =
    { url : String }


type alias Model =
    { photos : List Photo
    , selectedUrl : String
    }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"

update : Message -> Model -> Model
update message model =
    if message.operation == "SELECT_PHOTO" then
        { model | selectedUrl = message.data }
    else
        model

view: Model -> Html Message
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img
            [ src (urlPrefix ++ "large/" ++ model.selectedUrl)
            , class "large"
            ]
            []
        ]

viewThumbnail : String -> Photo -> Html Message
viewThumbnail selectedUrl thumbnail =
    img
        [ src (urlPrefix ++ thumbnail.url)
        , classList [ ( "selected", selectedUrl == thumbnail.url ) ]
        , onClick { operation = "SELECT_PHOTO", data = thumbnail.url }
        ]
        []


initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


main =
    Html.beginnerProgram { model = initialModel, view = view, update = update }
