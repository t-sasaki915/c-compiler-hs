module LexicalAnalyser (lexicalAnalyse) where

import           Constant
import           LexicalAnalyseException
import           Token

type AnalyseResult = Either LexicalAnalyseException [Token]

data State = State
  { tokens               :: [Token]
  , wordAnalyseMemory    :: String
  , commentAnalyseMemory :: String
  , analysingWord        :: Bool
  , analysingComment     :: Bool
  , index                :: Int
  }

withNewTokens :: [Token] -> (State -> [Token] -> State) -> State -> State
withNewTokens ts f s = f s (tokens s ++ ts)
withNewToken :: Token -> (State -> [Token] -> State) -> State -> State
withNewToken t = withNewTokens [t]
withPreviousTokens :: (State -> [Token] -> State) -> State -> State
withPreviousTokens = withNewTokens []

withMoreWordAnalyseMemory :: Char -> (State -> [Token] -> String -> State) -> State -> [Token] -> State
withMoreWordAnalyseMemory c f s ts = f s ts (wordAnalyseMemory s ++ [c])
withPreviousWordAnalyseMemory :: (State -> [Token] -> String -> State) -> State -> [Token] -> State
withPreviousWordAnalyseMemory f s ts = f s ts (wordAnalyseMemory s)
withClearedWordAnalyseMemory :: (State -> [Token] -> String -> State) -> State -> [Token] -> State
withClearedWordAnalyseMemory f s ts = f s ts ""

withMoreCommentAnalyseMemory :: Char -> (State -> [Token] -> String -> String -> State) -> State -> [Token] -> String -> State
withMoreCommentAnalyseMemory c f s ts wm = f s ts wm (commentAnalyseMemory s ++ [c])
withClearedCommentAnalyseMemory :: (State -> [Token] -> String -> String -> State) -> State -> [Token] -> String -> State
withClearedCommentAnalyseMemory f s ts wm = f s ts wm ""

withWordAnalyseMode :: (State -> [Token] -> String -> String -> Bool -> State) -> State -> [Token] -> String -> String -> State
withWordAnalyseMode f s ts wm cm = f s ts wm cm True
withoutWordAnalyseMode :: (State -> [Token] -> String -> String -> Bool -> State) -> State -> [Token] -> String -> String -> State
withoutWordAnalyseMode f s ts wm cm = f s ts wm cm False
withPreviousWordAnalyseMode :: (State -> [Token] -> String -> String -> Bool -> State) -> State -> [Token] -> String -> String -> State
withPreviousWordAnalyseMode f s ts wm cm = f s ts wm cm (analysingWord s)

withCommentAnalyseMode :: (State -> [Token] -> String -> String -> Bool -> Bool -> State) -> State -> [Token] -> String -> String -> Bool -> State
withCommentAnalyseMode f s ts wm cm wa = f s ts wm cm wa True
withoutCommentAnalyseMode :: (State -> [Token] -> String -> String -> Bool -> Bool -> State) -> State -> [Token] -> String -> String -> Bool -> State
withoutCommentAnalyseMode f s ts wm cm wa = f s ts wm cm wa False

withNewIndex :: Int -> State -> [Token] -> String -> String -> Bool -> Bool -> State
withNewIndex i s ts wm cm wa ca = State ts wm cm wa ca (index s + i)
withNextIndex :: State -> [Token] -> String -> String -> Bool -> Bool -> State
withNextIndex = withNewIndex 1

lexicalAnalyse :: String -> AnalyseResult
lexicalAnalyse sourceCode = analyse $ State [] "" "" False False 0
  where
  analyse :: State -> AnalyseResult
  analyse state
    | reachedToBottom =
        case () of
          () | commentAnalyse ->
                 case () of
                   () | wordAnalyse -> Right $ tokens state ++ [Comment commentMemory, finaliseWordAnalyse]
                      | otherwise   -> Right $ tokens state ++ [Comment commentMemory]
             | wordAnalyse -> Right $ tokens state ++ [finaliseWordAnalyse]
             | otherwise -> Right $ tokens state
    | c `elem` whitespaces =
        case () of
          () | commentAnalyse ->
                 continueAnalysing $
                   withPreviousTokens $
                   withPreviousWordAnalyseMemory $
                   withMoreCommentAnalyseMemory c $
                   withPreviousWordAnalyseMode $
                   withCommentAnalyseMode
                   withNextIndex
             | wordAnalyse ->
                 continueAnalysing $
                   withNewTokens [finaliseWordAnalyse, Whitespace c] $
                   withClearedWordAnalyseMemory $
                   withClearedCommentAnalyseMemory $
                   withoutWordAnalyseMode $
                   withoutCommentAnalyseMode
                   withNextIndex
             | otherwise ->
                 continueAnalysing $
                   withNewToken (Whitespace c) $
                   withClearedWordAnalyseMemory $
                   withClearedCommentAnalyseMemory $
                   withoutWordAnalyseMode $
                   withoutCommentAnalyseMode
                   withNextIndex
    | c `elem` newLines =
        case () of
          () | commentAnalyse ->
                 case () of
                   () | wordAnalyse ->
                          continueAnalysing $
                            withNewTokens [Comment commentMemory, finaliseWordAnalyse, NewLine c] $
                            withClearedWordAnalyseMemory $
                            withClearedCommentAnalyseMemory $
                            withoutWordAnalyseMode $
                            withoutCommentAnalyseMode
                            withNextIndex
                      | otherwise ->
                          continueAnalysing $
                            withNewTokens [Comment commentMemory, NewLine c] $
                            withClearedWordAnalyseMemory $
                            withClearedCommentAnalyseMemory $
                            withoutWordAnalyseMode $
                            withoutCommentAnalyseMode
                            withNextIndex
             | wordAnalyse ->
                 continueAnalysing $
                   withNewTokens [finaliseWordAnalyse, NewLine c] $
                   withClearedWordAnalyseMemory $
                   withClearedCommentAnalyseMemory $
                   withoutWordAnalyseMode $
                   withoutCommentAnalyseMode
                   withNextIndex
             | otherwise ->
                 continueAnalysing $
                   withNewToken (NewLine c) $
                   withClearedWordAnalyseMemory $
                   withClearedCommentAnalyseMemory $
                   withoutWordAnalyseMode $
                   withoutCommentAnalyseMode
                   withNextIndex
    | c `elem` symbols =
        case () of
          () | commentAnalyse ->
                 continueAnalysing $
                   withPreviousTokens $
                   withPreviousWordAnalyseMemory $
                   withMoreCommentAnalyseMemory c $
                   withPreviousWordAnalyseMode $
                   withCommentAnalyseMode
                   withNextIndex
             | wordAnalyse ->
                 continueAnalysing $
                   withNewTokens [finaliseWordAnalyse, Symbol c] $
                   withClearedWordAnalyseMemory $
                   withClearedCommentAnalyseMemory $
                   withoutWordAnalyseMode $
                   withoutCommentAnalyseMode
                   withNextIndex
             | otherwise ->
                 continueAnalysing $
                   withNewToken (Symbol c) $
                   withClearedWordAnalyseMemory $
                   withClearedCommentAnalyseMemory $
                   withoutWordAnalyseMode $
                   withoutCommentAnalyseMode
                   withNextIndex
    | c `elem` letters =
        case () of
          () | commentAnalyse ->
                 continueAnalysing $
                   withPreviousTokens $
                   withPreviousWordAnalyseMemory $
                   withMoreCommentAnalyseMemory c $
                   withPreviousWordAnalyseMode $
                   withCommentAnalyseMode
                   withNextIndex
             | otherwise ->
                 continueAnalysing $
                   withPreviousTokens $
                   withMoreWordAnalyseMemory c $
                   withClearedCommentAnalyseMemory $
                   withWordAnalyseMode $
                   withoutCommentAnalyseMode
                   withNextIndex
    | c `elem` digits =
        case () of
          () | commentAnalyse ->
                 continueAnalysing $
                   withPreviousTokens $
                   withPreviousWordAnalyseMemory $
                   withMoreCommentAnalyseMemory c $
                   withPreviousWordAnalyseMode $
                   withCommentAnalyseMode
                   withNextIndex
             | otherwise ->
                 continueAnalysing $
                   withPreviousTokens $
                   withMoreWordAnalyseMemory c $
                   withClearedCommentAnalyseMemory $
                   withWordAnalyseMode $
                   withoutCommentAnalyseMode
                   withNextIndex
    | otherwise =
        case () of
          () | commentAnalyse ->
                 continueAnalysing $
                   withPreviousTokens $
                   withPreviousWordAnalyseMemory $
                   withMoreCommentAnalyseMemory c $
                   withPreviousWordAnalyseMode $
                   withCommentAnalyseMode
                   withNextIndex
             | otherwise ->
                 Left $ UnexpectedCharacter index' sourceCode c
    where
    index' = index state
    wordAnalyse = analysingWord state
    commentAnalyse = analysingComment state
    wordMemory = wordAnalyseMemory state
    commentMemory = commentAnalyseMemory state

    reachedToBottom = index' >= length sourceCode
    c = sourceCode !! index'

    continueAnalysing :: (State -> State) -> AnalyseResult
    continueAnalysing f = analyse $ f state

    finaliseWordAnalyse :: Token
    finaliseWordAnalyse
      | wordMemory `elem` keywords     = Keyword wordMemory
      | all (`elem` digits) wordMemory = Number wordMemory
      | otherwise                      = Identifier wordMemory

