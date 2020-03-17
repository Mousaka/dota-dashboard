module Main exposing (..)

import Browser
import Element
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html, div, h1, img, text)
import Http
import Json.Decode as JD
import Json.Encode as JE



---- MODEL ----


type alias Model =
    { usernameInputField : String
    , steamIdInputField : String
    , apiError : Maybe String
    , users : List UserStats
    , disableAddUser : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { usernameInputField = "", steamIdInputField = "", apiError = Nothing, users = [], disableAddUser = False }, getWinLoseLastWeek )



---- UPDATE ----


type Msg
    = UsernameChanged String
    | SteamIdChanged String
    | AddButtonClicked
    | AddUserResponse (Result Http.Error ())
    | WinLoseLastWeekResponse (Result Http.Error (List UserStats))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsernameChanged input ->
            ( { model | usernameInputField = input }, Cmd.none )

        SteamIdChanged input ->
            ( { model | steamIdInputField = input }, Cmd.none )

        AddButtonClicked ->
            ( { model | disableAddUser = True }, addUser model )

        AddUserResponse result ->
            case result of
                Ok _ ->
                    ( { model | apiError = Nothing, usernameInputField = "", steamIdInputField = "", disableAddUser = False }, getWinLoseLastWeek )

                Err _ ->
                    ( { model | apiError = Just "Error with connection to api", disableAddUser = False }, Cmd.none )

        WinLoseLastWeekResponse result ->
            case result of
                Ok users ->
                    ( { model | apiError = Nothing, users = users }, Cmd.none )

                Err error ->
                    ( { model | apiError = Just "Error with connection to api" }, Cmd.none )


type alias UserStats =
    { username : String
    , steamId : String
    , win : Int
    , lose : Int
    }


userStatsDecoder : JD.Decoder UserStats
userStatsDecoder =
    JD.map4 UserStats
        (JD.field "username" JD.string)
        (JD.field "steamId" JD.string)
        (JD.field "win" JD.int)
        (JD.field "lose" JD.int)


getWinLoseLastWeek : Cmd Msg
getWinLoseLastWeek =
    Http.get
        { url = "https://mousaka-dota-dashboad.builtwithdark.com/api/users"
        , expect = Http.expectJson WinLoseLastWeekResponse (JD.list userStatsDecoder)
        }


addUser : Model -> Cmd Msg
addUser model =
    Http.post
        { url = "https://mousaka-dota-dashboad.builtwithdark.com/api/add-user"
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "username", JE.string model.usernameInputField )
                    , ( "steamId", JE.string model.steamIdInputField )
                    ]
                )
        , expect = Http.expectWhatever AddUserResponse
        }



---- VIEW ----


addMeView : Model -> Element.Element Msg
addMeView model =
    Element.column [ Element.alignBottom, Element.padding 10, Element.spacing 10 ]
        [ Element.el []
            (Element.text "Add me to the list")
        , Element.Input.text
            []
            { onChange = UsernameChanged
            , text = model.usernameInputField
            , placeholder = Nothing
            , label = Element.Input.labelAbove [] (Element.el [ Element.Font.size 14 ] (Element.text "Username"))
            }
        , Element.Input.text []
            { onChange = SteamIdChanged
            , text = model.steamIdInputField
            , placeholder = Nothing
            , label = Element.Input.labelAbove [] (Element.el [ Element.Font.size 14 ] (Element.text "SteamId"))
            }
        , Element.Input.button [ Element.Border.rounded 3, Element.Border.width 1 ]
            { onPress =
                if model.disableAddUser then
                    Nothing

                else
                    Just AddButtonClicked
            , label =
                Element.el [ Element.padding 5 ]
                    (if model.disableAddUser then
                        Element.text "adding..."

                     else
                        Element.text "Add me"
                    )
            }
        ]


viewUsers : List UserStats -> Element.Element Msg
viewUsers users =
    Element.column [ Element.paddingXY 0 10, Element.height Element.fill ]
        (List.map
            (\u ->
                Element.row [ Element.spacing 10 ]
                    [ Element.text u.username
                    , Element.text (String.fromInt u.win ++ " wins")
                    , Element.text (String.fromInt u.lose ++ " losses")
                    ]
            )
            users
        )


view : Model -> Html Msg
view model =
    Element.layout [ Element.padding 50 ] <|
        Element.column [ Element.width Element.fill, Element.height Element.fill ]
            [ Element.row []
                [ Element.el [ Element.Font.bold ] (Element.text "Last 7 days of dota ") ]
            , viewUsers model.users
            , addMeView model
            ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
