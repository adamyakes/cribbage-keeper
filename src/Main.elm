port module Main exposing (main)

import Browser
import Html
import Html.Attributes as A
import Html.Events as E
import Icons
import String

main : Program () Model Msg
main = Browser.document
  { view = view
  , update = update
  , init = init
  , subscriptions = always Sub.none
  }

port setDialog : (String, String) -> Cmd msg

dialogId : String
dialogId = "edit-dialog"

closeDialog : Cmd Msg
closeDialog = setDialog (dialogId, "close")

openDialog : Cmd Msg
openDialog = setDialog (dialogId, "open")

-- MODEL 

type alias Model = 
  { playerOneName : String
  , playerTwoName : String
  , resetting : Bool
  , round : Round
  , history : List RoundHistory
  , scores : List ScoreCount
  , dialog : Dialog
  , view : View
  , dealing : Player
  }

type alias ScoreCount = (Score, Int)

type RoundHistory
  = Incomplete Round
  | Complete Round

type Round
  = Pegging
    { playerOneScores : List ScoreCount
    , playerTwoScores : List ScoreCount
    }
  | Hand
    { playerOneHand : List ScoreCount
    , playerTwoHand : List ScoreCount
    , crib : List ScoreCount
    , dealer : Player
    , active : HandScoreState
    }
    
type HandScoreState
  = OppositeDealer
  | Dealer
  | Crib

newPegging : Round
newPegging = Pegging { playerOneScores = [], playerTwoScores = [] }

newHand : Player -> Round
newHand dealer = Hand { playerOneHand = [], playerTwoHand = [], crib = [], dealer = dealer, active = OppositeDealer }

type Dialog
  = NoDialog
  | EditPlayer Player

type View
  = ActionButtons
  | History

type Player
  = POne
  | PTwo

init : flags -> (Model, Cmd msg)
init _ = 
  let
    model =
      { playerOneName = "P1"
      , playerTwoName = "P2"
      , dealing = POne
      , round = newPegging
      , history = []
      , resetting = False
      , dialog = NoDialog
      , view = ActionButtons
      , scores = []
      }
  in
  (model, Cmd.none)

type Score 
  = Pair
  | ThreeOfKind
  | FourOfKind
  | Fifteen
  | RunThree
  | RunFour
  | RunFive
  | LastToLay
  | FlushFour
  | FlushFive
  | Nob
  | Heels
  | ThirtyOne

value : Score -> Int
value score =
  case score of 
    Pair ->
      2
    ThreeOfKind ->
      6
    FourOfKind ->
      12
    RunThree ->
      3
    RunFour ->
      4
    RunFive ->
      5
    Nob ->
      1
    Heels ->
      2
    Fifteen ->
      2
    FlushFive ->
      5
    FlushFour ->
      4
    LastToLay ->
      1
    ThirtyOne ->
      2

shortDescription : Score -> String
shortDescription score = 
  case score of
    Pair ->
      "P"
    ThreeOfKind ->
      "3K"
    FourOfKind ->
      "4K"
    RunThree ->
      "R3"
    RunFour ->
      "R4"
    RunFive ->
      "R5"
    Nob ->
      "N"
    Heels ->
      "H"
    Fifteen ->
      "15"
    ThirtyOne ->
      "31"
    LastToLay ->
      "Go"
    FlushFive ->
      "F5"
    FlushFour ->
      "F4"
    

type Msg
  = Reset
  | Cancel
  | GotScore Score
  | ClearScores
  | Undo
  | SetView View
  | StartEditPlayer Player
  | DialogDone
  | SetPlayerName Player String
  | SubmitPeg Player
  | PeggingComplete
  | SubmitHandOrCrib
  | SwapDealer


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset ->
      if model.resetting then
        init ()
      else
        ({ model | resetting = True }, Cmd.none)
    Cancel ->
      ({ model | resetting = False }, Cmd.none)
    GotScore score ->
      ({ model | scores = incScoreCount (score, 1) model.scores }, Cmd.none)
    ClearScores ->
      ({ model | scores = [] } , Cmd.none)
    SubmitPeg player ->
      let
        newRound =
          case (model.round, player) of
            (Pegging p, POne) ->
              Just <| Pegging { p | playerOneScores = combineScores model.scores p.playerOneScores }
            (Pegging p, PTwo) ->
              Just <| Pegging { p | playerTwoScores = combineScores model.scores p.playerTwoScores }
            (Hand _, _)  ->
              -- can't get a peg if we're counting hands
              Nothing
      in
      case newRound of
        Just r ->
          ({ model | scores = [] , round = r , history = Incomplete model.round :: model.history }, Cmd.none)
        Nothing ->
          (model, Cmd.none)
    SubmitHandOrCrib ->
      let
        newModel =
          case model.round of
            Pegging _ ->
              -- Ignore messages when we're pegging
              model
            Hand h ->
              case h.active of
                OppositeDealer ->
                  let 
                    newRound =
                      case h.dealer of
                        POne ->
                          Hand { h | playerTwoHand = model.scores, active = Dealer }
                        PTwo ->
                          Hand { h | playerOneHand = model.scores, active = Dealer }
                  in
                  { model | history = Incomplete model.round :: model.history, scores = [], round = newRound }
                Dealer ->
                  let
                    newRound =  
                      case h.dealer of
                        POne ->
                          Hand { h | playerOneHand = model.scores, active = Crib }
                        PTwo ->
                          Hand { h | playerTwoHand = model.scores, active = Crib }
                  in
                  { model | history = Incomplete model.round :: model.history, scores = [], round = newRound }
                Crib ->
                  { model
                  | history = Complete (Hand { h | crib = model.scores }) :: Incomplete model.round :: model.history
                  , scores = []
                  , round = newPegging
                  , dealing = otherPlayer model.dealing
                  }
      in
      (newModel, Cmd.none)
    PeggingComplete ->
      case model.round of
        Hand _ ->
          -- We shouldn't get this if we're not pegging
          (model, Cmd.none)
        Pegging p ->
          ({ model | history = Complete model.round :: model.history, round = newHand model.dealing }, Cmd.none)
    Undo ->
      let
        (newRound, newHistory) =
          case model.history of
            [] ->
              -- can't pop history if we don't have any
              (model.round, [])
            Incomplete round :: rest ->
              (round, rest)
            Complete round :: rest ->
              (round, rest)
        newModel =
          { model | round = newRound, history = newHistory, scores = [] }
      in
      (newModel, Cmd.none)
    SetView v ->
      ({ model | view = v }, Cmd.none)
    StartEditPlayer player ->
      ({ model | dialog = EditPlayer player }, openDialog)
    DialogDone ->
      ({ model | dialog = NoDialog }, closeDialog)
    SetPlayerName player name ->
      let
        newModel =
          case player of
            POne ->
              { model | playerOneName = name }
            PTwo ->
              { model | playerTwoName = name }
      in
        (newModel, Cmd.none)
    SwapDealer ->
      let
        -- Dealer is stored twice unfortunately
        newRound =
          case model.round of
            Pegging _ ->
              model.round
            Hand h ->
              Hand { h | dealer = otherPlayer h.dealer }
        newModel = 
          { model | dealing = otherPlayer model.dealing, round = newRound }
      in
      (newModel, Cmd.none)


otherPlayer : Player -> Player
otherPlayer player =
  case player of
    POne ->
      PTwo
    PTwo ->
      POne

incScoreCount : ScoreCount -> List ScoreCount -> List ScoreCount
incScoreCount (newScore, newScoreCount) scores =
  case scores of
    (score, count) :: rest ->
      if score == newScore then
        (score, count + newScoreCount) :: rest
      else
        (score, count) :: incScoreCount (newScore, newScoreCount) rest
    [] ->
      [(newScore, newScoreCount)]

combineScores : List ScoreCount -> List ScoreCount -> List ScoreCount
combineScores xs ys =
  case xs of
    x :: rest ->
      combineScores rest (incScoreCount x ys)
    [] ->
      ys

-- VIEW

view : Model -> Browser.Document Msg
view model =
  let
    p1p2 player =
      case player of
        POne ->
          "P1"
        PTwo ->
          "P2"
    submitScoreButtons =
      case model.round of
        Pegging _ ->
          [ Html.div
            [ A.class "score-buttons"]
            [ Html.button
              [ E.onClick (SubmitPeg POne) ]
              [ Html.text "Peg\nP1" ]
            ]
          , Html.div
            [ A.class "score-buttons"]
            [ Html.button
              [ E.onClick (SubmitPeg PTwo) ]
              [ Html.text "Peg\nP2" ]
            ]
          , Html.div
            [ A.class "score-buttons" ]
            [ Html.button
              [ E.onClick PeggingComplete, A.disabled <| not <| List.isEmpty model.scores ]
              [ Icons.done [] ]
            ]
          ]
        Hand h ->
          case h.active of
            OppositeDealer ->
              [ Html.div
                [ A.class "score-buttons" ]
                [ Html.button
                  [ E.onClick SubmitHandOrCrib ]
                  [ Html.text (p1p2 (otherPlayer h.dealer) ++ "'s Hand") ]
                ]
              ]
            Dealer ->
              [ Html.div
                [ A.class "score-buttons" ]
                [ Html.button
                  [ E.onClick SubmitHandOrCrib ]
                  [ Html.text (p1p2 h.dealer ++ "'s Hand") ]
                ]
              ]
            Crib ->
              [ Html.div 
                [ A.class "score-buttons" ]
                [ Html.button
                  [ E.onClick SubmitHandOrCrib ]
                  [ Html.text (p1p2 h.dealer ++ "'s Crib") ]
                ]
              ]
    viewButtonList scoreAndDescriptionList =
      let
        multiplierText score =
          if scoreCount score model.scores == 0 then
            ""
          else
            " x " ++ (String.fromInt <| scoreCount score model.scores)
        makeBtn (score, desc) =
          Html.button
            [ E.onClick (GotScore score)
            , A.class (if not <| String.isEmpty <| multiplierText score then "with-count" else "")
            ]
            [ Html.text (desc ++ multiplierText score)]
        buttons = 
          scoreAndDescriptionList
          |> List.map makeBtn
      in
      Html.div
        [ A.class "scoring-buttons" ]
        buttons

    mainView =
      case model.view of
        ActionButtons ->
          let
            scoreButtons =
              [ [ (Fifteen, "15")
                , (LastToLay, "Go")
                , (ThirtyOne, "31")
                ]
              , [ (Nob, "Nob")
                , (Heels, "Heels")
                ]
              , [ (Pair, "Pair")
                , (ThreeOfKind, "3 of kind")
                , (FourOfKind, "4 of a kind")
                ]
              , [ (RunThree, "Run of 3")
                , (RunFour, "Run of 4")
                , (RunFive, "Run of 5")
                ]
              , [ (FlushFour, "Flush of 4")
                , (FlushFive, "Flush of 5")
                ]
              ]
              |> List.map viewButtonList
            commandRow =
              Html.div
                [ A.class "scoring-buttons" ]
                ( clearOrUndo :: submitScoreButtons )
          in
          scoreButtons ++ [ commandRow ]
        History ->
          let
            { rows } =
              List.foldr
                displayRound
                { rows = [], playerOneTotal = 0, playerTwoTotal = 0, deal = 0 }
                (model.round :: List.filterMap getCompleteRounds model.history)
          in
          [ Html.table
            []
            [ Html.thead
              []
              [ Html.th [] [ Html.text "Deal" ]
              , Html.th [] [ Html.text "Hand" ]
              , Html.th [] [ Html.text "Player" ]
              , Html.th [] [ Html.text "Earned" ]
              , Html.th [] [ Html.text "Total" ]
              ]
            , Html.tbody
              []
              rows
            ]
          ]
    displayRound round { rows, playerOneTotal, playerTwoTotal, deal } =
      let
        header =
          case round of
            Pegging _ ->
              "Pegging"
            Hand _ ->
              "Hand"
        scoreCountStr (score, count) = 
          if count > 1 then
            String.fromInt count ++ " x " ++ shortDescription score 
          else
            shortDescription score

        allScores =
          case round of
            Pegging p ->
              { playerOnePrimary = p.playerOneScores
              , playerOneSecondary = []
              , playerTwoPrimary = p.playerTwoScores
              , playerTwoSecondary = []
              }
            Hand h ->
              { playerOnePrimary = h.playerOneHand
              , playerOneSecondary = if h.dealer == POne then h.crib else []
              , playerTwoPrimary = h.playerTwoHand
              , playerTwoSecondary = if h.dealer == PTwo then h.crib else []
              }
        paraScores scores =
          let
            scoreString =
              String.join ", " <| List.map scoreCountStr scores
            total =
              String.fromInt <| totalScores scores
            text = 
              if List.length scores > 0 then
                scoreString ++ " = " ++ total
              else
                "-"
          in
          Html.p [] [ Html.text text ]
        newRows =
          [ Html.tr
            []
            [ Html.td [ A.rowspan 2 ] [ Html.text <| String.fromInt newDeal]
            , Html.td [ A.rowspan 2 ] [ Html.text header ]
            , Html.td [] [ Html.text model.playerOneName ]
            , Html.td
              []
              [ paraScores allScores.playerOnePrimary
              , paraScores allScores.playerOneSecondary
              ]
            , Html.td [] [ Html.text <| String.fromInt (playerOneEarned + playerOneTotal) ]
            ]
          , Html.tr
            []
            [ Html.td [] [ Html.text model.playerTwoName ]
            , Html.td
              []
              [ paraScores allScores.playerTwoPrimary
              , paraScores allScores.playerTwoSecondary
              ]
            , Html.td [] [ Html.text <| String.fromInt (playerTwoEarned + playerTwoTotal) ]
            ]
          ]
        playerOneEarned = totalScores <| allScores.playerOnePrimary ++ allScores.playerOneSecondary
        playerTwoEarned = totalScores <| allScores.playerTwoPrimary ++ allScores.playerTwoSecondary
        newDeal =
          case round of
            Pegging _ ->
              deal + 1
            Hand _ ->
              deal
      in
      { rows = newRows ++ rows
      , playerOneTotal = playerOneEarned + playerOneTotal
      , playerTwoTotal = playerTwoEarned + playerTwoTotal
      , deal = newDeal
      }
    dialogView =
      let
        nameDialog player name =
          Html.node "dialog"
            [ A.id dialogId ]
            [ Html.form
              [E.onSubmit DialogDone ]
              [ Html.input
                [ A.value name
                , A.type_ "text"
                , E.onInput <| SetPlayerName player
                ]
                []
              , Html.input [ A.type_ "submit", A.value "OK" ] []
              ]
            ]
      in
      case model.dialog of
        NoDialog ->
          Html.node "dialog" [ A.id dialogId ] []
        EditPlayer POne ->
          nameDialog POne model.playerOneName
        EditPlayer PTwo ->
          nameDialog PTwo model.playerTwoName
    clearOrUndo =
      if List.isEmpty model.scores then
        Html.button
          [ E.onClick Undo
          , A.disabled (List.isEmpty model.history)
          ]
          [ Icons.undo [] ]
      else
        Html.button
          [ E.onClick ClearScores ]
          [ Html.text "Clear" ]
    body =
      [ Html.div
        [ A.class "score-head" ]
        [ Html.div [A.class "center-text", E.onClick (StartEditPlayer POne) ] [ Html.text <| model.playerOneName ]
        , Html.div [A.class "center-text"] [ Html.text <| String.fromInt <| scorePlayer POne model ]
        ]
      , Html.div
        [ A.class "score-head" ]
        [ Html.div [A.class "center-text", E.onClick (StartEditPlayer PTwo) ] [ Html.text <| model.playerTwoName ]
        , Html.div [A.class "center-text"] [ Html.text <| String.fromInt <| scorePlayer PTwo model ]
        ]
      , Html.div [ A.class "command-row" ]
        [ Html.button
          [ E.onClick (SetView ActionButtons)
          , A.disabled (model.view == ActionButtons)
          ]
          [ Icons.gridView [] ]
        , Html.button
          [ E.onClick (SetView History)
          , A.disabled (model.view == History)
          ]
          [ Icons.history [] ]
        , Html.button
          [ E.onClick SwapDealer ]
          [ Icons.swap [] ]
        ]
      , Html.div [ A.class "flex-fill"] mainView
      , dialogView
      ]
  in 
  { title = "Cribbage Scorer"
  , body = [ Html.div [ A.class "flex"] body ]
  }

scoreCount : Score -> List ScoreCount -> Int
scoreCount score lst =
  lst
  |> List.filter (\(s, _) -> s == score)
  |> List.head
  |> Maybe.map Tuple.second
  |> Maybe.withDefault 0

getCompleteRounds : RoundHistory -> Maybe Round
getCompleteRounds roundHistory =
  case roundHistory of
    Complete r -> 
      Just r
    Incomplete _ ->
      Nothing

scorePlayer : Player -> Model -> Int
scorePlayer player model = 
  let
    playerTotal round =
      case (round, player) of
        (Pegging { playerOneScores }, POne) ->
          playerOneScores
        (Pegging { playerTwoScores }, PTwo) ->
          playerTwoScores
        (Hand h, POne) ->
          h.playerOneHand ++ if h.dealer == POne then h.crib else []
        (Hand h, PTwo) ->
          h.playerTwoHand ++ if h.dealer == PTwo then h.crib else []
    scores = model.round :: List.filterMap getCompleteRounds model.history
  in
  scores 
  |> List.map (playerTotal >> totalScores)
  |> List.sum


-- Util

filterMaybes : List (Maybe a) -> List a
filterMaybes = List.filterMap identity

totalScores : List ScoreCount -> Int
totalScores = List.map (\(score, count) -> count * value score) >> List.sum 
