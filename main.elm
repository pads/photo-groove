-- Exports every global function.


module Main exposing (..)

-- Import the Array type from the Array module (avoids using Array.Array).

import Array exposing (Array)
import Html exposing (Html, button, div, h1, h3, label, img, input, p, text)


-- Import a specific list of functions. type is a reserved keyword hence the underscore.

import Html.Attributes exposing (id, class, classList, src, name, type_, title)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, string, int, list)
import Json.Decode.Pipeline exposing (decode, required, optional)


-- Import the Random module as is.

import Random


-- Assign a type to a specific kind of record.


type alias Photo =
    { url : String -- record key and it's type.
    , size : Int
    , title : String
    }


type alias Model =
    { photos : List Photo -- Photos can be a List where all values must be Photo records.
    , selectedUrl : Maybe String -- Maybe is a special type that says it an be Type X or Nothing.
    , loadingError : Maybe String
    , chosenSize : ThumbnailSize
    }



-- A union type - a type that can be represented as any of these types


type Message
    = SelectByUrl String -- Here, String is an argument type.
    | SelectByIndex Int
    | SurpriseMe
    | SetSize ThumbnailSize
    | LoadPhotos (Result Http.Error (List Photo))


type ThumbnailSize
    = Small
    | Medium
    | Large



-- The most simple function, takes no arguments and returns a string.


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


photoDecoder : Decoder Photo
photoDecoder =
    decode Photo
        -- pipes are syntatic sugar for applying multiple functions at once.
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"



{- This type definition says:
   Given a message, return a function that takes a model.
   That function takes a model and returns a model and command function
   (with message argument) tuple.
-}


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        SelectByUrl url ->
            -- Create a new model and assign the url, send no command.
            ( { model | selectedUrl = Just url }, Cmd.none )

        SelectByIndex index ->
            let
                -- This scopes functions to only exist inside this case statement.
                newSelectedUrl : Maybe String
                newSelectedUrl =
                    model.photos
                        |> Array.fromList
                        |> Array.get index
                        |> Maybe.map .url

                -- This is run first and is the same as: (\photo -> photo.url)
            in
                ( { model | selectedUrl = newSelectedUrl }, Cmd.none )

        SurpriseMe ->
            let
                randomPhotoPicker : Random.Generator Int
                randomPhotoPicker =
                    Random.int 0 (List.length model.photos - 1)
            in
                -- Random.generate is the command and SelectbyIndex is the message.
                ( model, Random.generate SelectByIndex randomPhotoPicker )

        SetSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        {- Because LoadPhotos expects a Result type argument you can split it
           out by result (response first then error)
        -}
        LoadPhotos (Ok photos) ->
            ( { model
                | photos = photos
                , selectedUrl = Maybe.map .url (List.head photos)
              }
            , Cmd.none
            )

        -- Underscore denotes that we don't care about the error type here (e.g. Timeout)
        LoadPhotos (Err _) ->
            ( { model | loadingError = Just "Error!" }, Cmd.none )


viewOrError : Model -> Html Message
viewOrError model =
    case model.loadingError of
        Nothing ->
            view model

        Just errorMessage ->
            div [ class "error-message" ]
                [ h1 [] [ text "Photo Groove" ]
                , p [] [ text errorMessage ]
                ]


view : Model -> Html Message
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick SurpriseMe ]
            [ text "Surprise Me!" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , viewLarge model.selectedUrl
        ]


viewLarge : Maybe String -> Html Message
viewLarge selectedUrl =
    case selectedUrl of
        Nothing ->
            text ""

        Just selectedUrl ->
            img [ class "large", src (urlPrefix ++ "large/" ++ selectedUrl) ] []


viewThumbnail : Maybe String -> Photo -> Html Message
viewThumbnail selectedUrl thumbnail =
    img
        [ src (urlPrefix ++ thumbnail.url)
        , title (thumbnail.title ++ " [" ++ toString thumbnail.size ++ " KB]")
        , classList [ ( "selected", selectedUrl == Just thumbnail.url ) ]
        , onClick (SelectByUrl thumbnail.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> Html Message
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (SetSize size) ] []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
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
    { photos = []
    , selectedUrl = Nothing
    , loadingError = Nothing
    , chosenSize = Medium
    }



-- Run once, on start up only.


initialCommand : Cmd Message
initialCommand =
    list photoDecoder
        |> Http.get "http://elm-in-action.com/photos/list.json"
        |> Http.send LoadPhotos



-- Our entry point into the app and is referred to in index.html


main : Program Never Model Message
main =
    Html.program
        { init = ( initialModel, initialCommand )
        , view = viewOrError
        , update = update
        , subscriptions = (\_ -> Sub.none) -- An example of an anonymous function with an unused argument.
        }
