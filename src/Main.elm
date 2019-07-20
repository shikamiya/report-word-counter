import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Json.Encode
import Json.Decode

-- MAIN

main =
  Browser.sandbox { 
    init = init, 
    update = update, 
    view = view
  }

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

init : Model
init =
  Model 
    Nothing
    ""
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

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateTypicalCount typicalCount ->
      { model | typicalCount = String.toInt typicalCount }
    UpdateContent title content ->
      let
        updateSection section =
          if section.title == title then
            { section | content = content }
          else
            section
        
        sections = List.map updateSection model.sections
      in
      { model | sections = sections }
    UpdateRatio title ratio_str ->
      let
        ratio = String.toInt ratio_str
        updateSection section =
          if section.title == title && ratio /= Nothing then
            { section | ratio = Maybe.withDefault(0) ratio }
          else
            section
        
        sections = List.map updateSection model.sections
      in
        { model | sections = sections }
    UpdateNextTitle title ->
      { model | nextTitle = title }
    AddSection ->
      let
        sections = List.append model.sections <| List.singleton { title = model.nextTitle, ratio = 1, content = "" }
      in
        { model | sections = sections, nextTitle = "" }
    RemoveSection title ->
      let
        sections = List.filter (\s -> s.title /= title) model.sections
      in
        { model | sections = sections }
      

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [] [ input [type_ "number", placeholder "文字数", value <| typicalCountStr model, onInput UpdateTypicalCount] [] ]
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
      , textarea [ cols 100, rows 15, placeholder section.title, onInput <| UpdateContent section.title] []
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
