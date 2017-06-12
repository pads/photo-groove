module Main exposing (..)

import Array exposing (Array)
import Html exposing (Html, button, div, h1, h3, label, img, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Message =
    { operation : String, data : String }


type alias Photo =
    { url : String }


type alias Model =
    { photos : List Photo
    , selectedUrl : String
    , chosenSize: ThumbnailSize
    }


type ThumbnailSize
    = Small
    | Medium
    | Large


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


update : Message -> Model -> Model
update message model =
    case message.operation of
        "SELECT_PHOTO" ->
            { model | selectedUrl = message.data }

        "SURPRISE_ME" ->
            { model | selectedUrl = "2.jpeg" }

        _ ->
            model


view : Model -> Html Message
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick { operation = "SURPRISE_ME", data = "" } ]
            [ text "Surprise Me!" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ] )
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
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


viewSizeChooser: ThumbnailSize -> Html Message
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size" ] []
        , text (sizeToString size)
        ]


sizeToString: ThumbnailSize -> String
sizeToString size =
      case size of
          Small ->
              "small"
          Medium ->
              "medium"
          Large ->
              "large"


initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    , chosenSize = Medium
    }


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


main =
    Html.beginnerProgram { model = initialModel, view = view, update = update }
