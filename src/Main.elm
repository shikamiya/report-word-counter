module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Json.Encode as E
import Json.Decode as D
import Dict

import Bootstrap.CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Select as Select
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Radio as Radio
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing

import Cacher exposing (cache)

-- Port Utilities

encodeModel : Model -> E.Value
encodeModel model =
  E.object
    [ ("typicalCount", E.int <| Maybe.withDefault 0 model.typicalCount)
    , ("sections", E.list encodeSection model.sections)
    ]

encodeSection : Section -> E.Value
encodeSection section =
  E.object
    [ ("title", E.string section.title)
    , ("ratio", E.int section.ratio)
    , ("content", E.string section.content)
    ]

decodeModel : D.Decoder (Maybe Model)
decodeModel =
  D.maybe (D.map2 (\typicalCount -> \sections -> { typicalCount = Just typicalCount, nextTitle = "", sections = sections })
    (D.field "typicalCount" D.int)
    (D.field "sections" (D.list decodeSection)))

decodeSection : D.Decoder Section
decodeSection =
  D.map3 Section 
    (D.field "title" D.string)
    (D.field "ratio" D.int)
    (D.field "content" D.string)

-- MAIN

main =
  Browser.element { 
    init = init, 
    update = update, 
    view = view,
    subscriptions = subscriptions
  }

subscriptions : Model -> Sub Msg
subscriptions model =
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
  , sections : List Section
  }

init : Maybe String -> (Model, Cmd msg)
init flags =
  let
    maybeModel = Maybe.withDefault Nothing <| Maybe.map (\f -> Result.withDefault Nothing (D.decodeString decodeModel f)) flags
  in
    case maybeModel of
      Just model ->
        (model, Cmd.none)
      _ ->
        (Model Nothing "" [], Cmd.none)

verifyTypicalCount : Model -> Maybe Int
verifyTypicalCount model =
  let
    count = Maybe.withDefault 0 model.typicalCount
  in
    if 0 < count then Just count else Nothing

sumOfAllConentLength : Model -> Int
sumOfAllConentLength model = List.sum <| List.map (\s -> String.length s.content) model.sections

sumOfRatio : Model -> Maybe Int
sumOfRatio model =
  let
    sum = List.sum <| List.map (\s -> s.ratio) model.sections
  in
    if sum == 0 then Nothing else Just sum

typicalCountPerRatio : Model -> Float
typicalCountPerRatio model =
  Maybe.withDefault 1.0 <| Maybe.map2 (\x -> \y -> (toFloat x) / (toFloat y)) (verifyTypicalCount model) <| sumOfRatio model

-- UPDATE

type Msg
  = UpdateTypicalCount String
  | UpdateContent String String
  | UpdateRatio String String
  | UpdateNextTitle String
  | AddSection
  | RemoveSection String
  | InitSections

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    UpdateTypicalCount typicalCount ->
      let
        new_model = { model | typicalCount = String.toInt typicalCount }
      in
        (new_model, Cmd.batch [ cache <| encodeModel new_model ])
    UpdateContent title content ->
      let
        updateSection section =
          if section.title == title then
            { section | content = content }
          else
            section
        
        sections = List.map updateSection model.sections
        new_model = { model | sections = sections }
      in
      (new_model, Cmd.batch [ cache <| encodeModel new_model ])
    UpdateRatio title ratio_str ->
      let
        ratio = String.toInt ratio_str
        updateSection section =
          if section.title == title && ratio /= Nothing then
            { section | ratio = Maybe.withDefault(0) ratio }
          else
            section
        
        sections = List.map updateSection model.sections
        new_model = { model | sections = sections }
      in
        (new_model, Cmd.batch [ cache <| encodeModel new_model ])
    UpdateNextTitle title ->
      ({ model | nextTitle = title }, Cmd.none)
    AddSection ->
      let
        sections = List.append model.sections <| List.singleton { title = model.nextTitle, ratio = 1, content = "" }
        new_model = { model | sections = sections, nextTitle = "" }
      in
        (new_model, Cmd.batch [ cache <| encodeModel new_model ])
    RemoveSection title ->
      let
        sections = List.filter (\s -> s.title /= title) model.sections
      in
        ({ model | sections = sections }, Cmd.none)
    InitSections ->
      ({ model | sections =
        [
          { title = "提示"
          , ratio = 0
          , content = ""
          },
          { title = "要約"
          , ratio = 35
          , content = ""
          },
          { title = "全体"
          , ratio = 15
          , content = ""
          },
          { title = "議論"
          , ratio = 35
          , content = ""
          },
          { title = "まとめ"
          , ratio = 15
          , content = ""
          }
        ]
      }, Cmd.none)
      

-- VIEW

view : Model -> Html Msg
view model =
  Grid.container []
    [ Bootstrap.CDN.stylesheet
    , Grid.containerFluid []
        [ Grid.row []
          [ Grid.col [ Col.sm2 ] [ label [ for "typical_count" ] [ text "総文字数" ] ]
          , Grid.col [ Col.sm2 ] [ Input.number [ Input.attrs [ id "typical_count" ], Input.value <| Maybe.withDefault "" <| Maybe.map String.fromInt model.typicalCount, Input.onInput UpdateTypicalCount] ]
          , Grid.col [ Col.sm4 ] []
          , Grid.col [ Col.sm4 ] [ Button.button [ Button.outlineInfo, Button.onClick InitSections ] [ text "リセットして初期のセクションを追加する" ] ]
          ]
        ]
    , div [] <| List.map (\x -> viewInput (typicalCountPerRatio model) x) model.sections
    , Grid.containerFluid []
      [ Grid.row []
        [ Grid.col [ Col.md, Col.lg6 ]
          [ Form.formInline []
            [ InputGroup.config
              (InputGroup.text [ Input.attrs [ id "next_title" ], Input.value model.nextTitle, Input.onInput UpdateNextTitle ])
              |> InputGroup.predecessors [ InputGroup.span [] [ text "追加するセクションのタイトル" ] ]
              |> InputGroup.successors [ InputGroup.button [ Button.info, Button.onClick AddSection ] [ text "追加"] ]
              |> InputGroup.view
            ]
          ]
        ]
      , Grid.row []
          [ Grid.col [ Col.sm4 ]
            [ text "総文字数："
            , text <| String.fromInt <| sumOfAllConentLength model
            , text "/"
            , text (Maybe.withDefault (Maybe.withDefault "0" <| Maybe.map String.fromInt <| sumOfRatio model) (Maybe.map String.fromInt <| verifyTypicalCount model))
            , text "("
            , text <| toStringWithSign <| (sumOfAllConentLength model) - (Maybe.withDefault 0 model.typicalCount)
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

viewInput : Float -> Section -> Html Msg
viewInput countPerRatio section =
  let
    contentLength = String.length section.content
  in
    Card.config []
      |> Card.header []
        [ Grid.containerFluid []
          [ Grid.row []
            [ Grid.col [ Col.md10, Col.lg11 ]
              [ text section.title
              ]
            , Grid.col [ Col.md2, Col.lg1 ]
              [ Button.button [ Button.small, Button.outlineDanger, Button.attrs [ Spacing.ml2 ], Button.onClick <| RemoveSection section.title] [ text "Delete" ]
              ]
            ]
          ]
        ]
      |> Card.block []
        [
          Block.custom <| Form.form []
            [ Textarea.textarea [ Textarea.rows 15, Textarea.onInput <| UpdateContent section.title, Textarea.value section.content ]
            , let
                limit = floor <| countPerRatio * toFloat section.ratio
                diff = contentLength - limit
              in
                Grid.containerFluid []
                  [ Grid.row []
                    [ Grid.col [ Col.sm4]
                      [ text <| String.fromInt contentLength
                      , text "/"
                      , text <| String.fromInt limit
                      , text " ("
                      , text <| toStringWithSign diff
                      , text ")"
                      ]
                    , Grid.col [ Col.sm4 ] []
                    , Grid.col [ Col.sm4 ]
                      [ InputGroup.config (InputGroup.number [ Input.value <| String.fromInt section.ratio, Input.onInput <| UpdateRatio section.title ])
                        |> InputGroup.predecessors [ InputGroup.span [] [ text "割合" ] ]
                        |> InputGroup.view
                      ]
                    ]
                  ]
            ]
        ]
      |> Card.view

toStringWithSign : Int -> String
toStringWithSign num =
  if num > 0 then
    String.append "+" <| String.fromInt num
  else
    String.fromInt num
