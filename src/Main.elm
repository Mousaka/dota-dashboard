module Main exposing (..)

import Browser
import Element
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
import Http
import Json.Decode as JD
import Json.Decode.Extra as JD
import Json.Encode as JE
import Round



---- MODEL ----


type alias Model =
    { steamIdInputField : String
    , apiError : Maybe String
    , users : List UserStats
    , disableAddUser : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { steamIdInputField = "", apiError = Nothing, users = [], disableAddUser = False }, getWinLoseLastWeek )



---- UPDATE ----


type Msg
    = SteamIdChanged String
    | AddButtonClicked
    | AddUserResponse (Result Http.Error ())
    | WinLoseLastWeekResponse (Result Http.Error (List UserStats))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SteamIdChanged input ->
            ( { model | steamIdInputField = input }, Cmd.none )

        AddButtonClicked ->
            ( { model | disableAddUser = True }, addUser model )

        AddUserResponse result ->
            case result of
                Ok _ ->
                    ( { model | apiError = Nothing, steamIdInputField = "", disableAddUser = False }, getWinLoseLastWeek )

                Err _ ->
                    ( { model | apiError = Just "Error with connection to api", disableAddUser = False }, Cmd.none )

        WinLoseLastWeekResponse result ->
            case result of
                Ok users ->
                    ( { model | apiError = Nothing, users = users }, Cmd.none )

                Err error ->
                    ( { model | apiError = Just "Error with connection to api" }, Cmd.none )


type alias Match =
    { kills : Int
    , deaths : Int
    , assists : Int
    }


type alias Kda =
    { kills : Int
    , deaths : Int
    , assists : Int
    }


type alias UserStats =
    { username : String
    , steamId : String
    , avatar : String
    , win : Int
    , lose : Int
    , matches : List Match
    , kda : Kda
    }


kda : Int -> Int -> Int -> Float
kda kills deaths assists =
    if kills + assists == 0 then
        toFloat 0

    else if deaths == 0 then
        toFloat (kills + assists)

    else
        toFloat (kills + assists) / toFloat deaths


bestKdaSort : List UserStats -> List UserStats
bestKdaSort userStats =
    userStats
        |> List.sortBy
            (\u ->
                kda u.kda.kills u.kda.deaths u.kda.assists
            )
        |> List.reverse


bestWinRateSort : List UserStats -> List UserStats
bestWinRateSort userStats =
    userStats
        |> List.sortBy
            (\u ->
                toFloat u.win / toFloat u.lose
            )
        |> List.reverse


matchDecoder : JD.Decoder Match
matchDecoder =
    JD.map3 Match
        (JD.field "kills" JD.int)
        (JD.field "deaths" JD.int)
        (JD.field "assists" JD.int)


userStatsDecoder : JD.Decoder UserStats
userStatsDecoder =
    let
        calcKda : List Match -> Kda
        calcKda matches =
            List.foldl (\m acc -> { acc | kills = acc.kills + m.kills, deaths = acc.deaths + m.deaths, assists = acc.assists + m.assists })
                { kills = 0, deaths = 0, assists = 0 }
                matches
    in
    JD.succeed UserStats
        |> JD.andMap (JD.field "username" JD.string)
        |> JD.andMap (JD.field "steamId" JD.string)
        |> JD.andMap (JD.field "avatar" JD.string)
        |> JD.andMap (JD.field "win" JD.int)
        |> JD.andMap (JD.field "lose" JD.int)
        |> JD.andMap (JD.field "matches" (JD.list matchDecoder))
        |> JD.andMap (JD.field "matches" (JD.list matchDecoder) |> JD.map calcKda)


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
                    [ ( "steamId", JE.string model.steamIdInputField )
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


avatar : String -> Element.Element Msg
avatar url =
    Element.image [ Element.height (Element.fill |> Element.maximum 100) ] { src = url, description = "avatar" }


viewTopUserWinLoss : UserStats -> Element.Element Msg
viewTopUserWinLoss userStats =
    if userStats.win > 0 && userStats.win // userStats.lose > 0 then
        Element.row [ Element.spacing 10, Element.Font.extraBold ]
            [ avatar userStats.avatar
            , Element.text ("🔥" ++ userStats.username ++ "🔥")
            , Element.text (String.fromInt userStats.win ++ " wins")
            , Element.text (String.fromInt userStats.lose ++ " losses")
            ]

    else
        viewUserWinLoss userStats


viewUserWinLoss : UserStats -> Element.Element Msg
viewUserWinLoss userStats =
    Element.row [ Element.spacing 10 ]
        [ avatar userStats.avatar
        , Element.text userStats.username
        , Element.text (String.fromInt userStats.win ++ " wins")
        , Element.text (String.fromInt userStats.lose ++ " losses")
        ]


viewWinLossRatio : List UserStats -> Element.Element Msg
viewWinLossRatio users =
    let
        sortedUsers =
            users |> bestWinRateSort
    in
    Element.column [ Element.paddingXY 0 10, Element.height Element.fill ]
        [ Element.text "Wins vs Losses"
        , Element.column []
            (case sortedUsers of
                [] ->
                    []

                h :: t ->
                    viewTopUserWinLoss h
                        :: List.map
                            viewUserWinLoss
                            t
            )
        ]


viewTopUserKda : UserStats -> Element.Element Msg
viewTopUserKda userStats =
    if (userStats.kda.kills + userStats.kda.assists) - userStats.kda.deaths > 0 then
        Element.row [ Element.spacing 10, Element.Font.extraBold ]
            [ avatar userStats.avatar
            , Element.column [ Element.spacing 5 ]
                [ Element.text ("🔥" ++ userStats.username ++ "🔥")
                , Element.row [] [ Element.text ((Round.round 2 <| kda userStats.kda.kills userStats.kda.deaths userStats.kda.assists) ++ " KDA") ]
                , Element.row []
                    [ Element.text (String.fromInt userStats.kda.kills ++ " / ")
                    , Element.text (String.fromInt userStats.kda.deaths ++ " / ")
                    , Element.text (String.fromInt userStats.kda.assists)
                    ]
                ]
            ]

    else
        viewUserKda userStats


viewUserKda : UserStats -> Element.Element Msg
viewUserKda userStats =
    Element.row [ Element.spacing 10 ]
        [ avatar userStats.avatar
        , Element.column [ Element.spacing 5 ]
            [ Element.text userStats.username
            , Element.row [] [ Element.text ((Round.round 2 <| kda userStats.kda.kills userStats.kda.deaths userStats.kda.assists) ++ " KDA") ]
            , Element.row []
                [ Element.text (String.fromInt userStats.kda.kills ++ " / ")
                , Element.text (String.fromInt userStats.kda.deaths ++ " / ")
                , Element.text (String.fromInt userStats.kda.assists)
                ]
            ]
        ]


viewKDA : List UserStats -> Element.Element Msg
viewKDA users =
    let
        sortedUsers =
            users |> bestKdaSort
    in
    Element.column [ Element.paddingXY 0 10, Element.height Element.fill ]
        [ Element.text "Kills + Assists / Deaths"
        , Element.column []
            (case sortedUsers of
                [] ->
                    []

                h :: t ->
                    viewTopUserKda h
                        :: List.map
                            viewUserKda
                            t
            )
        ]


view : Model -> Html Msg
view model =
    Element.layout [ Element.padding 50 ] <|
        Element.column [ Element.width Element.fill, Element.height Element.fill ]
            [ Element.row []
                [ Element.el [ Element.Font.bold ] (Element.text "This week of dota") ]
            , Element.row [ Element.spacing 40 ] [ viewWinLossRatio model.users, viewKDA model.users ]
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
