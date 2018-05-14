module MessageParser
  ( parseNewUser
  , parseMessage
  ) where

import qualified Text.Read     as Read

parseNewUser :: String -> Maybe Int
parseNewUser txt =
  verifyPattern (stripSurrounders "#") txt >>=
    verifyPattern (stripSurrounders "{{}}") >>=
      verifyPattern (stripSurrounders " ") >>=
        Read.readMaybe

parseMessage :: String -> Maybe (Int, String)
parseMessage text =
  let
    (recipientId : msgs) = splitOn ':' text
    mbRecipientId = parseNewUser recipientId
  in
  case (mbRecipientId, concat msgs) of
    (Just recId, ' ' : msg : rest) -> Just (recId, msg : rest)
    _                               -> Nothing

verifyPattern :: (String -> String) -> String -> Maybe String
verifyPattern stripFn text
  | length (stripFn text) == length text = Nothing
  | otherwise = Just (stripFn text)

splitOn :: Char -> String -> [String]
splitOn char xs =
  let (chunk, rest) = foldr go ("", []) xs
  in chunk : rest
  where
    go x (chunk, acc)
      | char == x = ("", chunk : acc)
      | otherwise = (x : chunk, acc)

stripSurrounders :: String -> String -> String
stripSurrounders toStrip xs =
  let toMid = ceiling $ fromIntegral (length toStrip) / 2
      leadingSurrounders = take toMid toStrip
      trailingSurrounders = takeLast toMid toStrip
  in if take toMid xs == leadingSurrounders && takeLast toMid xs == trailingSurrounders
       then dropLast toMid . drop toMid $ xs
       else xs
  where
    dropLast n xs = take (length xs - n) xs
    takeLast n xs = drop (length xs - n) xs
