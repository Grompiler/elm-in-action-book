port module PhotoGallery exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (checked, class, classList, id, name, src, title, type_)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode exposing (Decoder, Value, at, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Random


main : Program Float Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Float -> ( Model, Cmd Msg )
init flags =
    let
        activity =
            "Inintializing Pasta v" ++ String.fromFloat flags
    in
    ( { initialModel | activity = activity }, initialCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    activityChanges GotActivity


type alias Model =
    { status : Status
    , activity : String
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type ThumbnailSize
    = Small
    | Medium
    | Large


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))
    | SlidHue Int
    | SlidRipple Int
    | SlidNoise Int
    | GotActivity String


port setFilters : FilterOptions -> Cmd msg


port activityChanges : (String -> msg) -> Sub msg



-- port activityChanges : (Value -> msg) -> Sub msg


type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            applyFilters { model | status = selectUrl url model.status }

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored errorMessage ->
                    ( model, Cmd.none )

        GotPhotos (Ok photos) ->
            case photos of
                first :: rest ->
                    applyFilters
                        { model
                            | status =
                                case List.head photos of
                                    Just photo ->
                                        Loaded photos photo.url

                                    Nothing ->
                                        Loaded [] ""
                        }

                [] ->
                    ( { model | status = Errored "0 photo found" }, Cmd.none )

        GotPhotos (Err httpError) ->
            ( { model | status = Errored "Server error !" }, Cmd.none )

        SlidHue hue ->
            applyFilters { model | hue = hue }

        SlidRipple ripple ->
            applyFilters { model | ripple = ripple }

        SlidNoise noise ->
            applyFilters { model | noise = noise }

        GotActivity activity ->
            ( { model | activity = activity }, Cmd.none )


applyFilters : Model -> ( Model, Cmd msg )
applyFilters model =
    case model.status of
        Loaded photos selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
            ( model, setFilters { url = url, filters = filters } )

        Loading ->
            ( model, Cmd.none )

        Errored errorMessage ->
            ( model, Cmd.none )


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { -- url = "http://127.0.0.1:7878/photos/list"
          url = "https://elm-in-action.com/photos/list.json"

        -- , expect = Http.expectString GotPhotos
        , expect = Http.expectJson GotPhotos (Json.Decode.list photoDecoder)

        -- , expect = Http.expectString (\result -> GotPhotos result) why ?
        }


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        Errored errorMessage ->
            status


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser chosenSize size =
    label []
        [ input [ type_ "radio", name "size", checked (chosenSize == size), onClick (ClickedSize size) ] []
        , text (sizeToString size)
        ]


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model

            Loading ->
                [ text "Loading..." ]

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" (Encode.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]


viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl model =
    [ text "Photo Groove"
    , button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , div [ class "activity" ] [ text model.activity ]
    , div [ class "filters" ]
        [ viewFilter SlidHue "Hue" model.hue
        , viewFilter SlidRipple "Ripple" model.ripple
        , viewFilter SlidNoise "Noise" model.noise
        ]
    , h3 [] [ text "Choose Size" ]
    , div [ id "choose-size" ]
        (List.map (viewSizeChooser model.chosenSize) [ Small, Medium, Large ])
    , div
        [ id "thumbnails"
        , class (sizeToString model.chosenSize)
        ]
        (List.map (viewThumbnail selectedUrl) photos)
    , canvas
        [ class "large"

        -- , src (urlPrefix ++ "large/" ++ selectedUrl)
        , id "main-canvas"
        ]
        []
    ]


initialModel : Model
initialModel =
    { status = Loading
    , activity = ""
    , chosenSize = Medium
    , hue = 0
    , ripple = 0
    , noise = 0
    }


sizeToString : ThumbnailSize -> String
sizeToString thumbnailSize =
    case thumbnailSize of
        Small ->
            "small"

        Medium ->
            "medium"

        Large ->
            "large"


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl photo =
    img
        [ src (urlPrefix ++ photo.url)
        , title (photo.title ++ " [" ++ String.fromInt photo.size ++ "  KB] ")
        , classList [ ( "selected", selectedUrl == photo.url ) ]
        , onClick (ClickedPhoto photo.url)
        ]
        []


urlPrefix : String
urlPrefix =
    "https://elm-in-action.com/"


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children


onSlide : (Int -> Msg) -> Attribute Msg
onSlide toMsg =
    at [ "detail", "userSlidTo" ] int
        |> Json.Decode.map toMsg
        |> on "slide"
