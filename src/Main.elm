module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class, classList, href)
import Html.Lazy exposing (lazy)
import PhotoFolders as Folders
import PhotoGallery as Gallery
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s, string)


type alias Model =
    { page : Page, key : Nav.Key, version : Float }


type Route
    = Gallery
    | Folders
    | SelectedPhoto String


type Page
    = GalleryPage Gallery.Model
    | FoldersPage Folders.Model
    | NotFound


view : Model -> Document Msg
view model =
    let
        content =
            case model.page of
                FoldersPage folders ->
                    Folders.view folders
                        |> Html.map GotFoldersMsg

                GalleryPage gallery ->
                    Gallery.view gallery
                        |> Html.map GotGalleryMsg

                NotFound ->
                    text "Not Found"
    in
    { title = "Photo Groove, SPA style"
    , body =
        [ lazy viewHeader model.page
        , content
        , viewFooter
        ]
    }


init : Float -> Url -> Nav.Key -> ( Model, Cmd Msg )
init version url key =
    updateUrl url { page = NotFound, key = key, version = version }


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    case Parser.parse parser url of
        Just Gallery ->
            Gallery.init model.version
                |> toGallery model

        Just Folders ->
            Folders.init Nothing
                |> toFolders model

        Just (SelectedPhoto filename) ->
            Folders.init (Just filename)
                |> toFolders model

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )



-- ( { page = urlToPage version url, key = key, version = version }, Cmd.none )
-- urlToPage : Float -> Url -> Page
-- urlToPage version url =
--     case Parser.parse parser url of
--         Just Gallery ->
--             GalleryPage (Tuple.first (Gallery.init version))
--         Just Folders ->
--             FoldersPage (Tuple.first (Folders.init Nothing))
--         Just (SelectedPhoto filename) ->
--             FoldersPage (Tuple.first (Folders.init (Just filename)))
--         Nothing ->
--             NotFound


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Folders Parser.top
        , Parser.map Gallery (Parser.s "gallery")
        , Parser.map SelectedPhoto (Parser.s "photos" </> Parser.string)
        ]


viewHeader : Page -> Html Msg
viewHeader page =
    let
        logo =
            h1 [] [ text "Photo Groove" ]

        links =
            ul []
                [ navLink Folders { url = "/", caption = "Folders" }
                , navLink Gallery { url = "/gallery", caption = "Gallery" }
                ]

        navLink : Route -> { url : String, caption : String } -> Html Msg
        navLink route { url, caption } =
            li
                [ classList
                    [ ( "active"
                      , isActive { link = route, page = page }
                      )
                    ]
                ]
                [ a [ href url ] [ text caption ] ]
    in
    nav [] [ logo, links ]


isActive : { link : Route, page : Page } -> Bool
isActive { link, page } =
    case ( link, page ) of
        ( Gallery, GalleryPage _ ) ->
            True

        ( Gallery, _ ) ->
            False

        ( Folders, FoldersPage _ ) ->
            True

        ( Folders, _ ) ->
            False

        ( SelectedPhoto _, _ ) ->
            False


viewFooter : Html msg
viewFooter =
    footer [] [ text "One is never alone with a rubber duck. -Douglas Adams" ]


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotFoldersMsg Folders.Msg
    | GotGalleryMsg Gallery.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

        ChangedUrl url ->
            updateUrl url model

        GotGalleryMsg galleryMsg ->
            case model.page of
                GalleryPage gallery ->
                    toGallery model (Gallery.update galleryMsg gallery)

                _ ->
                    ( model, Cmd.none )

        GotFoldersMsg foldersMsg ->
            case model.page of
                FoldersPage folders ->
                    toFolders model (Folders.update foldersMsg folders)

                _ ->
                    ( model, Cmd.none )


toFolders : Model -> ( Folders.Model, Cmd Folders.Msg ) -> ( Model, Cmd Msg )
toFolders model ( folders, cmd ) =
    ( { model | page = FoldersPage folders }, Cmd.map GotFoldersMsg cmd )


toGallery : Model -> ( Gallery.Model, Cmd Gallery.Msg ) -> ( Model, Cmd Msg )
toGallery model ( gallery, cmd ) =
    ( { model | page = GalleryPage gallery }, Cmd.map GotGalleryMsg cmd )


main : Program Float Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        GalleryPage gallery ->
            Gallery.subscriptions gallery
                |> Sub.map GotGalleryMsg

        _ ->
            Sub.none
