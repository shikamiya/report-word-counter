module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Json.Encode as E
import Json.Decode as D
import Dict
import Cacher exposing (cache)

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

init : String -> (Model, Cmd msg)
init flags =
  let
    maybeModel = Result.withDefault Nothing (D.decodeString decodeModel flags)
  in
    case maybeModel of
      Just model ->
        (model, Cmd.none)
      _ ->
        (Model Nothing "" [], Cmd.none)
    

typicalCountStr : Model -> String
typicalCountStr model = Maybe.withDefault "" <| Maybe.map String.fromInt model.typicalCount

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
  Maybe.withDefault 1.0 <| Maybe.map2 (\x -> \y -> (toFloat x) / (toFloat y)) model.typicalCount <| sumOfRatio model

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
  div []
    [ div [] [ input [type_ "number", placeholder "文字数", value <| typicalCountStr model, onInput UpdateTypicalCount] [] ]
    , div [] [ button [ Html.Events.onClick InitSections ] [ text "リセットして初期のセクションを追加する" ] ]
    , div [] <| List.map (\x -> viewInput (typicalCountPerRatio model) x) model.sections
    , div []
      [ text "追加するセクションのタイトル："
      , input [ type_ "text", placeholder "title", value model.nextTitle, onInput UpdateNextTitle ] []
      , button [ Html.Events.onClick AddSection ] [ text "追加" ]
      ]
    , div []
      [ text "総文字数："
      , text <| String.fromInt <| sumOfAllConentLength model
      , text "/"
      , text <| typicalCountStr model
      , text "("
      , text <| toStringWithSign <| (sumOfAllConentLength model) - (Maybe.withDefault 0 model.typicalCount)
      , text ")"
      ]
    , div [] [ text "コピー用" ]
    , div [] <| List.map (\x -> p [] [ text x.content ]) model.sections
    ]

viewInput : Float -> Section -> Html Msg
viewInput countPerRatio section =
  let
    contentLength = String.length section.content
  in
    div []
      [ div []
        [ text section.title
        , button [ Html.Events.onClick <| RemoveSection section.title] [ text "x" ]
        ]
      , textarea [ cols 100, rows 15, placeholder section.title, onInput <| UpdateContent section.title] [ text section.content ]
      , let
          limit = floor <| countPerRatio * toFloat section.ratio
          diff = contentLength - limit
        in
          div []
            [ text <| String.fromInt contentLength
            , text "/"
            , text <| String.fromInt limit
            , text " ("
            , text <| toStringWithSign diff
            , text ")"
            , text " 割合："
            , input [ type_ "number", value <| String.fromInt section.ratio, onInput <| UpdateRatio section.title ] []
            ]
    ]

toStringWithSign : Int -> String
toStringWithSign num =
  if num > 0 then
    String.append "+" <| String.fromInt num
  else
    String.fromInt num
