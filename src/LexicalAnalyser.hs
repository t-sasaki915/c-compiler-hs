{-# LANGUAGE TemplateHaskell #-}

module LexicalAnalyser (lexicalAnalyse) where

import           Constant
import           LexicalAnalyseException
import           Token
import           Util

import           Control.Lens            hiding (index)

type AnalyseResult = Either LexicalAnalyseException [Token]

data State = State
  { _tokens                     :: [Token]
  , _wordAnalyseMemory          :: String
  , _commentAnalyseMemory       :: String
  , _analysingWord              :: Bool
  , _analysingComment           :: Bool
  , _analysingSingleLineComment :: Bool
  , _index                      :: Int
  }

makeLenses ''State

lexicalAnalyse :: String -> AnalyseResult
lexicalAnalyse sourceCode = analyse $ State [] "" "" False False True 0
  where
  analyse :: State -> AnalyseResult
  analyse state
    | reachedToBottom =
        case () of
          () | commentAnalyse ->
                 case () of
                   () | not singleLineComment ->
                          Left $ UnclosingComment (index' - 1) sourceCode
                      | wordAnalyse ->
                          finaliseWordAnalyseAnd $ \wordToken ->
                            Right $ _tokens state ++ [Comment commentMemory, wordToken]
                      | otherwise ->
                          Right $ _tokens state ++ [Comment commentMemory]
             | wordAnalyse ->
                 finaliseWordAnalyseAnd $ \wordToken ->
                   Right $ _tokens state ++ [wordToken]
             | otherwise ->
                 Right $ _tokens state
    | c `elem` whitespaces =
        case () of
          () | commentAnalyse ->
                 continueAnalysing $
                   over commentAnalyseMemory (++[c]) .
                   over index (+1)
             | wordAnalyse ->
                 finaliseWordAnalyseAnd $ \wordToken ->
                   continueAnalysing $
                     over tokens (++ [wordToken, Whitespace c]) .
                     set wordAnalyseMemory "" .
                     set analysingWord False .
                     over index (+1)
             | otherwise ->
                 continueAnalysing $
                   over tokens (++[Whitespace c]) .
                   over index (+1)
    | c `elem` newLines =
        case () of
          () | commentAnalyse ->
                 case () of
                   () | not singleLineComment ->
                          continueAnalysing $
                            over commentAnalyseMemory (++[c]) .
                            over index (+1)
                      | wordAnalyse ->
                          finaliseWordAnalyseAnd $ \wordToken ->
                            continueAnalysing $
                              over tokens (++ [Comment commentMemory, wordToken, NewLine c]) .
                              set wordAnalyseMemory "" .
                              set commentAnalyseMemory "" .
                              set analysingWord False .
                              set analysingComment False .
                              over index (+1)
                      | otherwise ->
                          continueAnalysing $
                            over tokens (++ [Comment commentMemory, NewLine c]) .
                            set commentAnalyseMemory "" .
                            set analysingComment False .
                            over index (+1)
             | wordAnalyse ->
                 finaliseWordAnalyseAnd $ \wordToken ->
                   continueAnalysing $
                     over tokens (++ [wordToken, NewLine c]) .
                     set wordAnalyseMemory "" .
                     set analysingWord False .
                     over index (+1)
             | otherwise ->
                 continueAnalysing $
                   over tokens (++ [NewLine c]) .
                   over index (+1)
    | c `elem` symbols =
        case () of
          () | commentAnalyse ->
                 case () of
                   () | not singleLineComment ->
                          case () of
                            () | c == '*' ->
                                   case sourceCode !? (index' + 1) of
                                     Just '/' ->
                                       continueAnalysing $
                                         over tokens (++ [Comment $ commentMemory ++ "*/"]) .
                                         set commentAnalyseMemory "" .
                                         set analysingComment False .
                                         over index (+2)
                                     _ ->
                                       continueAnalysing $
                                         over commentAnalyseMemory (++[c]) .
                                         over index (+1)
                               | otherwise ->
                                   continueAnalysing $
                                     over commentAnalyseMemory (++[c]) .
                                     over index (+1)
                      | otherwise ->
                          continueAnalysing $
                            over commentAnalyseMemory (++[c]) .
                            over index (+1)
             | c == '/' ->
                 case sourceCode !? (index' + 1) of
                   Just '/' ->
                     continueAnalysing $
                       over commentAnalyseMemory (++ "//") .
                       set analysingComment True .
                       set analysingSingleLineComment True .
                       over index (+2)
                   Just '*' ->
                     continueAnalysing $
                       over commentAnalyseMemory (++ "/*") .
                       set analysingComment True .
                       set analysingSingleLineComment False .
                       over index (+2)
                   _ ->
                     case () of
                       () | wordAnalyse ->
                              finaliseWordAnalyseAnd $ \wordToken ->
                                continueAnalysing $
                                  over tokens (++ [wordToken, Symbol c]) .
                                  set wordAnalyseMemory "" .
                                  set analysingWord False .
                                  over index (+1)
                          | otherwise ->
                              continueAnalysing $
                                over tokens (++ [Symbol c]) .
                                over index (+1)
             | wordAnalyse ->
                 finaliseWordAnalyseAnd $ \wordToken ->
                   continueAnalysing $
                     over tokens (++ [wordToken, Symbol c]) .
                     set wordAnalyseMemory "" .
                     set analysingWord False .
                     over index (+1)
             | otherwise ->
                 continueAnalysing $
                   over tokens (++ [Symbol c]) .
                   over index (+1)
    | c `elem` letters =
        case () of
          () | commentAnalyse ->
                 continueAnalysing $
                   over commentAnalyseMemory (++[c]) .
                   over index (+1)
             | otherwise ->
                 continueAnalysing $
                   over wordAnalyseMemory (++[c]) .
                   set analysingWord True .
                   over index (+1)
    | c `elem` digits =
        case () of
          () | commentAnalyse ->
                 continueAnalysing $
                   over commentAnalyseMemory (++[c]) .
                   over index (+1)
             | otherwise ->
                 continueAnalysing $
                   over wordAnalyseMemory (++[c]) .
                   set analysingWord True .
                   over index (+1)
    | otherwise =
        case () of
          () | commentAnalyse ->
                 continueAnalysing $
                   over commentAnalyseMemory (++[c]) .
                   over index (+1)
             | otherwise ->
                 Left $ UnexpectedCharacter index' sourceCode c
    where
    index' = _index state
    wordAnalyse = _analysingWord state
    commentAnalyse = _analysingComment state
    singleLineComment = _analysingSingleLineComment state
    wordMemory = _wordAnalyseMemory state
    commentMemory = _commentAnalyseMemory state

    reachedToBottom = index' >= length sourceCode
    c = sourceCode !! index'

    continueAnalysing :: (State -> State) -> AnalyseResult
    continueAnalysing f = analyse $ f state

    finaliseWordAnalyseAnd :: (Token -> AnalyseResult) -> AnalyseResult
    finaliseWordAnalyseAnd f
      | wordMemory `elem` keywords =
          f $ Keyword wordMemory
      | any (`isPrefixOf'` wordMemory) digits =
          case () of
            () | matchesPerfectly numberFormat wordMemory -> f $ Number wordMemory
               | otherwise -> Left $ InvalidNumberFormat (index' - 1) sourceCode wordMemory
      | otherwise =
          f $ Identifier wordMemory

