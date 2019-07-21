module Main exposing (ModalKind(..), Model, Msg(..), Section, decodeModel, decodeSection, encodeModel, encodeSection, getModalVisibility, init, main, subscriptions, sumOfAllConentLength, sumOfRatio, toStringWithSign, typicalCountPerRatio, update, verifyTypicalCount, view, viewInput)

import Bootstrap.Button as Button
import Bootstrap.CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Cacher exposing (cache)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Json.Decode as D
import Json.Encode as E



-- Port Utilities


encodeModel : Model -> E.Value
encodeModel model =
    E.object
        [ ( "typicalCount", E.int <| Maybe.withDefault 0 model.typicalCount )
        , ( "sections", E.list encodeSection model.sections )
        ]


encodeSection : Section -> E.Value
encodeSection section =
    E.object
        [ ( "title", E.string section.title )
        , ( "ratio", E.int section.ratio )
        , ( "content", E.string section.content )
        ]


decodeModel : D.Decoder (Maybe Model)
decodeModel =
    D.maybe
        (D.map2 (\typicalCount -> \sections -> { typicalCount = Just typicalCount, nextTitle = "", modalKind = NoModal, sections = sections })
            (D.field "typicalCount" D.int)
            (D.field "sections" (D.list decodeSection))
        )


decodeSection : D.Decoder Section
decodeSection =
    D.map3 Section
        (D.field "title" D.string)
        (D.field "ratio" D.int)
        (D.field "content" D.string)



-- MAIN


main : Program (Maybe String) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MODEL


type alias Section =
    { title : String
    , ratio : Int
    , content : String
    }


type alias Model =
    { typicalCount : Maybe Int
    , nextTitle : String
    , modalKind : ModalKind
    , sections : List Section
    }


type ModalKind
    = NoModal
    | ResetModal
    | DeleteModal String


init : Maybe String -> ( Model, Cmd msg )
init flags =
    let
        maybeModel =
            Maybe.withDefault Nothing <| Maybe.map (\f -> Result.withDefault Nothing (D.decodeString decodeModel f)) flags
    in
    case maybeModel of
        Just model ->
            ( model, Cmd.none )

        _ ->
            ( Model Nothing "" NoModal [], Cmd.none )


verifyTypicalCount : Model -> Maybe Int
verifyTypicalCount model =
    let
        count =
            Maybe.withDefault 0 model.typicalCount
    in
    if 0 < count then
        Just count

    else
        Nothing


sumOfAllConentLength : Model -> Int
sumOfAllConentLength model =
    List.sum <| List.map (\s -> String.length s.content) model.sections


sumOfRatio : Model -> Maybe Int
sumOfRatio model =
    let
        sum =
            List.sum <| List.map (\s -> s.ratio) model.sections
    in
    if sum == 0 then
        Nothing

    else
        Just sum


typicalCountPerRatio : Model -> Float
typicalCountPerRatio model =
    Maybe.withDefault 1.0 <| Maybe.map2 (\x -> \y -> toFloat x / toFloat y) (verifyTypicalCount model) <| sumOfRatio model


getModalVisibility : ModalKind -> ModalKind -> Modal.Visibility
getModalVisibility target current =
    if target == current then
        Modal.shown

    else
        Modal.hidden



-- UPDATE


type Msg
    = UpdateTypicalCount String
    | UpdateContent String String
    | UpdateRatio String String
    | UpdateNextTitle String
    | AddSection
    | RemoveSection String
    | InitSections
    | ShowModal ModalKind
    | IgnoreModal


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        UpdateTypicalCount typicalCount ->
            let
                new_model =
                    { model | typicalCount = String.toInt typicalCount }
            in
            ( new_model, Cmd.batch [ cache <| encodeModel new_model ] )

        UpdateContent title content ->
            let
                updateSection section =
                    if section.title == title then
                        { section | content = content }

                    else
                        section

                sections =
                    List.map updateSection model.sections

                new_model =
                    { model | sections = sections }
            in
            ( new_model, Cmd.batch [ cache <| encodeModel new_model ] )

        UpdateRatio title ratio_str ->
            let
                ratio =
                    String.toInt ratio_str

                updateSection section =
                    if section.title == title && ratio /= Nothing then
                        { section | ratio = Maybe.withDefault 0 ratio }

                    else
                        section

                sections =
                    List.map updateSection model.sections

                new_model =
                    { model | sections = sections }
            in
            ( new_model, Cmd.batch [ cache <| encodeModel new_model ] )

        UpdateNextTitle title ->
            ( { model | nextTitle = title }, Cmd.none )

        AddSection ->
            let
                sections =
                    List.append model.sections <| List.singleton { title = model.nextTitle, ratio = 1, content = "" }

                new_model =
                    { model | sections = sections, nextTitle = "" }
            in
            ( new_model, Cmd.batch [ cache <| encodeModel new_model ] )

        RemoveSection title ->
            let
                sections =
                    List.filter (\s -> s.title /= title) model.sections

                new_model =
                    { model | sections = sections, modalKind = NoModal }
            in
            ( new_model, Cmd.batch [ cache <| encodeModel new_model ] )

        InitSections ->
            let
                new_model =
                    { model
                        | sections =
                            [ { title = "提示"
                              , ratio = 0
                              , content = ""
                              }
                            , { title = "要約"
                              , ratio = 35
                              , content = ""
                              }
                            , { title = "全体"
                              , ratio = 15
                              , content = ""
                              }
                            , { title = "議論"
                              , ratio = 35
                              , content = ""
                              }
                            , { title = "まとめ"
                              , ratio = 15
                              , content = ""
                              }
                            ]
                        , modalKind = NoModal
                    }
            in
            ( new_model, Cmd.batch [ cache <| encodeModel new_model ] )

        ShowModal kind ->
            ( { model | modalKind = kind }, Cmd.none )

        IgnoreModal ->
            ( { model | modalKind = NoModal }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Grid.container []
        [ Bootstrap.CDN.stylesheet
        , Grid.containerFluid [ class "mt-4" ]
            [ Grid.row []
                [ Grid.col [ Col.sm4 ]
                    [ InputGroup.config (InputGroup.number [ Input.attrs [ id "typical_count" ], Input.value <| Maybe.withDefault "" <| Maybe.map String.fromInt model.typicalCount, Input.onInput UpdateTypicalCount ])
                        |> InputGroup.predecessors [ InputGroup.span [] [ text "総文字数" ] ]
                        |> InputGroup.view
                    ]
                , Grid.col [ Col.md2, Col.offsetMd6, Col.lg1, Col.offsetLg7 ] [ Button.button [ Button.outlineInfo, Button.onClick <| ShowModal ResetModal ] [ text "リセット" ] ]
                ]
            ]
        , Modal.config IgnoreModal
            |> Modal.hideOnBackdropClick True
            |> Modal.h3 [] [ text "セクションのリセット" ]
            |> Modal.body [] [ p [] [ text "全てのセクションを削除して初期化しますか？" ] ]
            |> Modal.footer []
                [ Button.button
                    [ Button.outlinePrimary
                    , Button.onClick IgnoreModal
                    ]
                    [ text "キャンセル" ]
                , Button.button
                    [ Button.outlineDanger
                    , Button.onClick InitSections
                    ]
                    [ text "リセット" ]
                ]
            |> Modal.view (getModalVisibility ResetModal model.modalKind)
        , div [] <| List.map (\x -> viewInput (typicalCountPerRatio model) model.modalKind x) model.sections
        , Grid.containerFluid [ class "mt-4" ]
            [ Grid.row []
                [ Grid.col [ Col.md, Col.lg6 ]
                    [ Form.formInline []
                        [ InputGroup.config
                            (InputGroup.text [ Input.attrs [ id "next_title" ], Input.value model.nextTitle, Input.onInput UpdateNextTitle ])
                            |> InputGroup.predecessors [ InputGroup.span [] [ text "追加するセクションのタイトル" ] ]
                            |> InputGroup.successors [ InputGroup.button [ Button.info, Button.onClick AddSection ] [ text "追加" ] ]
                            |> InputGroup.view
                        ]
                    ]
                ]
            , Grid.row []
                [ Grid.col [ Col.sm, Col.attrs [ class "my-2" ] ]
                    [ text "総文字数："
                    , text <| String.fromInt <| sumOfAllConentLength model
                    , text "/"
                    , text (Maybe.withDefault (Maybe.withDefault "0" <| Maybe.map String.fromInt <| sumOfRatio model) (Maybe.map String.fromInt <| verifyTypicalCount model))
                    , text "("
                    , text <| toStringWithSign <| sumOfAllConentLength model - Maybe.withDefault 0 model.typicalCount
                    , text ")"
                    ]
                ]
            , Grid.row []
                [ Grid.col [ Col.sm6 ]
                    [ h5 [] [ text "コピー用" ] ]
                ]
            , Grid.row []
                [ Grid.col [ Col.sm ] <| List.map (\x -> p [] [ text x.content ]) model.sections
                ]
            ]
        ]


viewInput : Float -> ModalKind -> Section -> Html Msg
viewInput countPerRatio modalKind section =
    let
        contentLength =
            String.length section.content
    in
    div []
        [ Card.config [ Card.attrs [ Spacing.mt4 ] ]
            |> Card.header []
                [ Grid.containerFluid []
                    [ Grid.row []
                        [ Grid.col [ Col.md8 ]
                            [ text section.title
                            ]
                        , Grid.col [ Col.md2, Col.offsetMd2, Col.lg1, Col.offsetLg3 ]
                            [ Button.button [ Button.small, Button.outlineDanger, Button.attrs [ Spacing.ml2 ], Button.onClick <| ShowModal <| DeleteModal section.title ] [ text "Delete" ]
                            ]
                        ]
                    ]
                ]
            |> Card.block []
                [ Block.custom <|
                    Form.form []
                        [ Textarea.textarea [ Textarea.rows 15, Textarea.onInput <| UpdateContent section.title, Textarea.value section.content ]
                        , let
                            limit =
                                floor <| countPerRatio * toFloat section.ratio

                            diff =
                                contentLength - limit
                          in
                          Grid.containerFluid []
                            [ Grid.row [ Row.attrs [ class "mt-2" ] ]
                                [ Grid.col [ Col.sm4 ]
                                    [ text <| String.fromInt contentLength
                                    , text "/"
                                    , text <| String.fromInt limit
                                    , text " ("
                                    , text <| toStringWithSign diff
                                    , text ")"
                                    ]
                                , Grid.col [ Col.sm4, Col.offsetSm4 ]
                                    [ InputGroup.config (InputGroup.number [ Input.value <| String.fromInt section.ratio, Input.onInput <| UpdateRatio section.title ])
                                        |> InputGroup.predecessors [ InputGroup.span [] [ text "割合" ] ]
                                        |> InputGroup.view
                                    ]
                                ]
                            ]
                        ]
                ]
            |> Card.view
        , Modal.config IgnoreModal
            |> Modal.hideOnBackdropClick True
            |> Modal.h3 [] [ text "セクションの削除" ]
            |> Modal.body []
                [ p []
                    [ text section.title
                    , text " セクションを削除します。"
                    ]
                ]
            |> Modal.footer []
                [ Button.button
                    [ Button.outlinePrimary
                    , Button.onClick IgnoreModal
                    ]
                    [ text "キャンセル" ]
                , Button.button
                    [ Button.outlineDanger
                    , Button.onClick <| RemoveSection section.title
                    ]
                    [ text "削除" ]
                ]
            |> Modal.view (getModalVisibility (DeleteModal section.title) modalKind)
        ]


toStringWithSign : Int -> String
toStringWithSign num =
    if num > 0 then
        String.append "+" <| String.fromInt num

    else
        String.fromInt num
