module SyntaxAnalyser (syntaxAnalyse) where

import           SyntaxAnalyseException
import           SyntaxTree
import           Token
import           Util                   (cartesian)

import           Data.Maybe             (fromJust)

type AnalyseResult = Either SyntaxAnalyseException SyntaxTree

data DeclarationAnalyseStep = AnalyseType
                            | AnalyseLabel
                            | AnalyseOpenParentheses
                            | AnalyseArgumentType
                            | AnalyseArgumentLabel
                            | AnalyseArgumentSeparator
                            | AnalyseCloseParentheses
                            | AnalyseOpenBracket
                            | AnalyseCloseBracket

data State = State
  { declarationList      :: [SyntaxTree]
  , declarationType      :: Maybe Token
  , declarationLabel     :: Maybe Token
  , declarationArgTypes  :: [Token]
  , declarationArgLabels :: [Token]
  , declarationStep      :: DeclarationAnalyseStep
  , index                :: Int
  }

confirmDecAnd :: (State -> [SyntaxTree] -> State) -> State -> State
confirmDecAnd f s =
  f s (declarationList s ++ [newDeclaration])
  where
  newDeclaration =
    Node Declaration
      (
        [ Node (DeclarationLabel $ fromJust (declarationLabel s)) []
        , Node (TypeSpecifier $ fromJust (declarationType s)) []
        ] ++ map makeArgTree (cartesian (declarationArgTypes s) (declarationArgLabels s))
      )

  makeArgTree arg =
    Node DeclarationArgument
      [ Node (TypeSpecifier $ fst arg) []
      , Node (DeclarationLabel $ snd arg) []
      ]
withPreviousDecs :: (State -> [SyntaxTree] -> State) -> State -> State
withPreviousDecs f s = f s (declarationList s)

setDecTypeAnd :: Token -> (State -> [SyntaxTree] -> Maybe Token -> State) -> State -> [SyntaxTree] -> State
setDecTypeAnd t f s st = f s st (Just t)
unsetDecTypeAnd :: (State -> [SyntaxTree] -> Maybe Token -> State) -> State -> [SyntaxTree] -> State
unsetDecTypeAnd f s st = f s st Nothing
withPreviousDecType :: (State -> [SyntaxTree] -> Maybe Token -> State) -> State -> [SyntaxTree] -> State
withPreviousDecType f s st = f s st (declarationType s)

setDecLabelAnd :: Token -> (State -> [SyntaxTree] -> Maybe Token -> Maybe Token -> State) -> State -> [SyntaxTree] -> Maybe Token -> State
setDecLabelAnd t f s st dt = f s st dt (Just t)
unsetDecLabelAnd :: (State -> [SyntaxTree] -> Maybe Token -> Maybe Token -> State) -> State -> [SyntaxTree] -> Maybe Token -> State
unsetDecLabelAnd f s st dt = f s st dt Nothing
withPreviousDecLabel :: (State -> [SyntaxTree] -> Maybe Token -> Maybe Token -> State) -> State -> [SyntaxTree] -> Maybe Token -> State
withPreviousDecLabel f s st dt = f s st dt (declarationLabel s)

addDecArgTypeAnd :: Token -> (State -> [SyntaxTree] -> Maybe Token -> Maybe Token -> [Token] -> State) -> State -> [SyntaxTree] -> Maybe Token -> Maybe Token -> State
addDecArgTypeAnd t f s st dt dl = f s st dt dl (declarationArgTypes s ++ [t])
clearDecArgTypesAnd :: (State -> [SyntaxTree] -> Maybe Token -> Maybe Token -> [Token] -> State) -> State -> [SyntaxTree] -> Maybe Token -> Maybe Token -> State
clearDecArgTypesAnd f s st dt dl = f s st dt dl []
withPreviousDecArgTypes :: (State -> [SyntaxTree] -> Maybe Token -> Maybe Token -> [Token] -> State) -> State -> [SyntaxTree] -> Maybe Token -> Maybe Token -> State
withPreviousDecArgTypes f s st dt dl = f s st dt dl (declarationArgTypes s)

addDecArgLabelAnd :: Token -> (State -> [SyntaxTree] -> Maybe Token -> Maybe Token -> [Token] -> [Token] -> State) -> State -> [SyntaxTree] -> Maybe Token -> Maybe Token -> [Token] -> State
addDecArgLabelAnd t f s st dt dl at = f s st dt dl at (declarationArgLabels s ++ [t])
clearDecArgLabelsAnd :: (State -> [SyntaxTree] -> Maybe Token -> Maybe Token -> [Token] -> [Token] -> State) -> State -> [SyntaxTree] -> Maybe Token -> Maybe Token -> [Token] -> State
clearDecArgLabelsAnd f s st dt dl at = f s st dt dl at []
withPreviousDecArgLabels :: (State -> [SyntaxTree] -> Maybe Token -> Maybe Token -> [Token] -> [Token] -> State) -> State -> [SyntaxTree] -> Maybe Token -> Maybe Token -> [Token] -> State
withPreviousDecArgLabels f s st dt dl at = f s st dt dl at (declarationArgLabels s)

withDecAnalyseStep :: DeclarationAnalyseStep -> (State -> [SyntaxTree] -> Maybe Token -> Maybe Token -> [Token] -> [Token] -> DeclarationAnalyseStep -> State) -> State -> [SyntaxTree] -> Maybe Token -> Maybe Token -> [Token] -> [Token] -> State
withDecAnalyseStep as f s st dt dl at al = f s st dt dl at al as
withPreviousDecAnalyseStep :: (State -> [SyntaxTree] -> Maybe Token -> Maybe Token -> [Token] -> [Token] -> DeclarationAnalyseStep -> State) -> State -> [SyntaxTree] -> Maybe Token -> Maybe Token -> [Token] -> [Token] -> State
withPreviousDecAnalyseStep f s st dt dl at al = f s st dt dl at al (declarationStep s)

withNextIndex :: State -> [SyntaxTree] -> Maybe Token -> Maybe Token -> [Token] -> [Token] -> DeclarationAnalyseStep -> State
withNextIndex s st dt dl at al as = State st dt dl at al as (index s + 1)

syntaxAnalyse :: [Token] -> AnalyseResult
syntaxAnalyse tokens = analyse $ State [] Nothing Nothing [] [] AnalyseType 0
  where
  analyse :: State -> AnalyseResult
  analyse state
    | reachedToBottom =
        Right $
          Node Program
            [ Node DeclarationList declarations
            ]
    | otherwise =
        case t of
          (Whitespace _) ->
            ignoreAndDoNothing

          (NewLine _) ->
            ignoreAndDoNothing

          (Comment _) ->
            ignoreAndDoNothing

          (Keyword keyword) ->
            case declarationStep' of
              AnalyseType ->
                case keyword of
                  "void" ->
                    continueAnalysing $
                      withPreviousDecs $
                      setDecTypeAnd t $
                      withPreviousDecLabel $
                      withPreviousDecArgTypes $
                      withPreviousDecArgLabels $
                      withDecAnalyseStep AnalyseLabel
                      withNextIndex
                  "int" ->
                    continueAnalysing $
                      withPreviousDecs $
                      setDecTypeAnd t $
                      withPreviousDecLabel $
                      withPreviousDecArgTypes $
                      withPreviousDecArgLabels $
                      withDecAnalyseStep AnalyseLabel
                      withNextIndex
                  _ ->
                    Left $ UnexpectedToken t "Type"

              AnalyseArgumentType ->
                case keyword of
                  "void" ->
                    continueAnalysing $
                      withPreviousDecs $
                      withPreviousDecType $
                      withPreviousDecLabel $
                      withPreviousDecArgTypes $
                      withPreviousDecArgLabels $
                      withDecAnalyseStep AnalyseCloseParentheses
                      withNextIndex
                  "int" ->
                    continueAnalysing $
                      withPreviousDecs $
                      withPreviousDecType $
                      withPreviousDecLabel $
                      addDecArgTypeAnd t $
                      withPreviousDecArgLabels $
                      withDecAnalyseStep AnalyseArgumentLabel
                      withNextIndex
                  _ ->
                    Left $ UnexpectedToken t "Type"

              _ ->
                contextualUnexpectedTokenHalt

          (Identifier _) ->
            case declarationStep' of
              AnalyseLabel ->
                continueAnalysing $
                  withPreviousDecs $
                  withPreviousDecType $
                  setDecLabelAnd t $
                  withPreviousDecArgTypes $
                  withPreviousDecArgLabels $
                  withDecAnalyseStep AnalyseOpenParentheses
                  withNextIndex

              AnalyseArgumentLabel ->
                continueAnalysing $
                  withPreviousDecs $
                  withPreviousDecType $
                  withPreviousDecLabel $
                  withPreviousDecArgTypes $
                  addDecArgLabelAnd t $
                  withDecAnalyseStep AnalyseArgumentSeparator
                  withNextIndex

              _ ->
                contextualUnexpectedTokenHalt

          (Symbol symbol) ->
            case declarationStep' of
              AnalyseOpenParentheses ->
                case symbol of
                  '(' ->
                    continueAnalysing $
                      withPreviousDecs $
                      withPreviousDecType $
                      withPreviousDecLabel $
                      withPreviousDecArgTypes $
                      withPreviousDecArgLabels $
                      withDecAnalyseStep AnalyseArgumentType
                      withNextIndex
                  _ ->
                    Left $ UnexpectedToken t "'('"

              AnalyseArgumentSeparator ->
                case symbol of
                  ',' ->
                    continueAnalysing $
                      withPreviousDecs $
                      withPreviousDecType $
                      withPreviousDecLabel $
                      withPreviousDecArgTypes $
                      withPreviousDecArgLabels $
                      withDecAnalyseStep AnalyseArgumentType
                      withNextIndex
                  ')' ->
                    continueAnalysing $
                      withPreviousDecs $
                      withPreviousDecType $
                      withPreviousDecLabel $
                      withPreviousDecArgTypes $
                      withPreviousDecArgLabels $
                      withDecAnalyseStep AnalyseOpenBracket
                      withNextIndex
                  _ ->
                    Left $ UnexpectedToken t "',' or ')'"

              AnalyseCloseParentheses ->
                case symbol of
                  ')' ->
                    continueAnalysing $
                      withPreviousDecs $
                      withPreviousDecType $
                      withPreviousDecLabel $
                      withPreviousDecArgTypes $
                      withPreviousDecArgLabels $
                      withDecAnalyseStep AnalyseOpenBracket
                      withNextIndex
                  _ ->
                    Left $ UnexpectedToken t "')'"

              AnalyseOpenBracket ->
                case symbol of
                  '{' ->
                    continueAnalysing $
                      withPreviousDecs $
                      withPreviousDecType $
                      withPreviousDecLabel $
                      withPreviousDecArgTypes $
                      withPreviousDecArgLabels $
                      withDecAnalyseStep AnalyseCloseBracket
                      withNextIndex
                  _ ->
                    Left $ UnexpectedToken t "'{'"

              AnalyseCloseBracket ->
                case symbol of
                  '}' ->
                    continueAnalysing $
                      confirmDecAnd $
                        unsetDecTypeAnd $
                        unsetDecLabelAnd $
                        clearDecArgTypesAnd $
                        clearDecArgLabelsAnd $
                        withDecAnalyseStep AnalyseType
                        withNextIndex
                  _ ->
                    Left $ UnexpectedToken t "'}'"

              _ ->
                contextualUnexpectedTokenHalt

          _ -> -- Number
            ignoreAndDoNothing
    where
    index' = index state
    declarations = declarationList state
    declarationStep' = declarationStep state

    reachedToBottom = index' >= length tokens
    t = tokens !! index'

    continueAnalysing :: (State -> State) -> AnalyseResult
    continueAnalysing f = analyse $ f state

    ignoreAndDoNothing :: AnalyseResult
    ignoreAndDoNothing =
      continueAnalysing $
        withPreviousDecs $
        withPreviousDecType $
        withPreviousDecLabel $
        withPreviousDecArgTypes $
        withPreviousDecArgLabels $
        withPreviousDecAnalyseStep
        withNextIndex

    contextualUnexpectedTokenHalt :: AnalyseResult
    contextualUnexpectedTokenHalt = Left $ UnexpectedToken t expectation
      where
        expectation =
          case declarationStep' of
            AnalyseType              -> "Type"
            AnalyseLabel             -> "Identifier"
            AnalyseOpenParentheses   -> "'('"
            AnalyseArgumentType      -> "Type"
            AnalyseArgumentLabel     -> "Identifier"
            AnalyseArgumentSeparator -> "',' or ')'"
            AnalyseCloseParentheses  -> "')'"
            AnalyseOpenBracket       -> "'{'"
            AnalyseCloseBracket      -> "'}'"

