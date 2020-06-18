{-# LANGUAGE ScopedTypeVariables #-}

module Combinators where

import Annotations
import Control.Monad
import Data.Foldable
import Result
import Utils ((<$$>))
import AST (AST)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

type Position = (Integer, Integer)

type Input = (String, Position)

type MatchedString = Maybe String

type ParsingResult a = Result (a, MatchedString, Input)

type Parser a = Input -> ParsingResult a

run :: Parser a -> Input -> ParsingResult a
run parser = parser

p_char :: Char -> Parser Char
p_char _ ([], _) = Failure "No more input"
p_char char_to_match (char:chars, (line_number, chars_consumed))
  | char == char_to_match = Success (char, Just [char], (chars, new_position))
  | otherwise =
    Failure $ "expected '" ++ [char_to_match] ++ "' but got '" ++ [char] ++ "'"
  where
    new_position =
      if char == '\n'
        then (line_number + 1, chars_consumed + 1)
        else (line_number, chars_consumed + 1)

-- infixr 7 >..>
(>..>) :: Parser a -> Parser b -> Parser (a, b)
(>..>) left right input = do
  (left_matched, left_matched_string, left_rest) <- run left input
  (right_matched, right_matched_string, right_rest) <- run right left_rest
  return
    ( (left_matched, right_matched)
    , left_matched_string <> right_matched_string
    , right_rest)

-- infixr 7 >..*
(>..*) :: Parser a -> Parser b -> Parser a
(>..*) left right = left >..> right |>> fst

-- infixr 7 >..*
(*..>) :: Parser a -> Parser b -> Parser b
(*..>) left right = left >..> right |>> snd

(<!>) :: Parser a -> ((a, MatchedString, Input) -> Bool) -> Parser a
(<!>) parser should_fail input =
  run parser input >>=
  (\result ->
     if should_fail result
       then Failure "FAIL"
       else Success result)

-- infixr 6 <|>
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) left right input =
  let l = run left input
      r = run right input
   in case (l, r) of
        (Success _, Success _) -> longer_match l r
        (Success _, Failure _) -> l
        _ -> r
  where
    longer_match left_result@(Success (_, _, (_, l_consumed))) right_result@(Success (_, _, (_, r_consumed)))
      | l_consumed > r_consumed = left_result
      | otherwise = right_result

-- infixr 8 |>>
(|>>) :: Parser a -> (a -> b) -> Parser b
(|>>) parser mapping input = full_mapping <$> run parser input
  where
    full_mapping (match, matched_string, rest) =
      (mapping match, matched_string, rest)

(|>>>) :: Parser t -> (t -> LexicalInfo -> AST a LexicalInfo) -> Parser (AST a LexicalInfo)
(|>>>) parser constructor input = do
  (l_comment, l_com_matched, rest) <- run p_comment input
  (matched, matched_s, rest2) <- annotate <$> run parser rest
  (r_comment, r_com_matched, rest3) <- run p_comment rest2
  return
    ( map_annotation
        (\a -> a {left_space = l_comment, right_space = r_comment})
        matched
    , mconcat [l_com_matched, matched_s, r_com_matched]
    , rest3)
  where
    annotate (match, matched_string, rest) =
      ( constructor match $
        LexicalInfo
          { original_string = matched_string
          , left_space = ""
          , right_space = ""
          , original_file = Nothing
          }
      , matched_string
      , rest)

p_comment =
  (p_whitespace_n |>> join) >..>
  (many
     ((p_whitespace_n >..> single_comment) >..> p_whitespace_n |>>
      (\((a, b), c) -> join a ++ b ++ join c)) |>>
   join) |>>
  uncurry (++)
  where
    single_comment =
      p_string "#" >..>
      (many (p_any <|> p_string " " <|> p_string "\t") |>> join) |>>
      uncurry (++)

-- infixr 9 <?>
(<?>) :: Parser a -> String -> Input -> Result (a, MatchedString, Input)
(<?>) parser name input =
  case run parser input of
    Success result -> Success result
    Failure _ ->
      Failure $ "expected '" ++ name ++ "', got '" ++ fst input ++ "'"

(<^>) :: Parser a -> Parser a -> Parser a
(<^>) left right input =
  case run left input of
    Success s -> Success s
    Failure _ -> case run right input of
      Success s2 -> Success s2
      Failure m -> Failure m

any_of :: [Parser a] -> Parser a
any_of (p:ps) = foldl (<|>) p ps

p_any_of :: [String] -> Parser String
p_any_of = foldl1 (<|>) . map p_string

p_any_of_fast :: [String] -> Parser String
p_any_of_fast = foldl1 (<^>) . map p_string

sequence_of :: [Parser Char] -> Parser String
sequence_of parsers = foldr' and_then_concat_results (last parsers |>> (:[])) $ init parsers
  where
    and_then_concat_results :: Parser Char -> Parser String -> Parser String
    and_then_concat_results r1 r2 = r1 >..> r2 |>> uncurry (:)

p_string :: String -> Parser String
p_string string = sequence_of char_parsers
  where
    char_parsers = map p_char string

optional :: Parser a -> Parser (Maybe a)
optional parser = inner_fn
  where
    inner_fn input =
      case run parser input of
        Success (matched, matched_string, rest) ->
          Success (Just matched, matched_string, rest)
        Failure _ -> Success (Nothing, Just "", input)


optional_with_default :: Monoid b => Parser b -> Parser b
optional_with_default parser = optional parser |>> fold

many :: Parser a -> Parser [a]
many parser = Success . many'
  where
    many' input =
      from_result ([], Just "", input) $
      run parser input <$$> \(matched, matched_string, rest) ->
        let (matched', matched_string', rest') = many' rest
         in (matched : matched', matched_string <> matched_string', rest')

many1_separated :: Parser a -> String -> Parser [a]
many1_separated parser separator = parser >..> many p_item |>> uncurry (:)
  where
    p_item = p_string separator *..> parser

many_separated :: Parser a -> String -> Parser [a]
many_separated parser separator =
  optional_with_default $ many1_separated parser separator

many1 :: Parser a -> Parser [a]
many1 parser = parser >..> many parser |>> uncurry (:)

between :: Parser b -> (Parser a, Parser c) -> Parser (a, b, c)
between parser (left, right) =
  left >..> parser >..> right |>> (\((a, b), c) -> (a, b, c))

p_many_while :: Parser a -> (a -> a -> Bool) -> Parser [a]
p_many_while p condition input = Success $ p_many_while' input [] (Just "")
  where
    p_many_while' inp acc s =
      case run p inp of
        Failure _ -> (acc, s, inp)
        Success (matched, string, rest) ->
          if safe_condition acc matched
            then p_many_while' rest (acc ++ [matched]) (s <> string)
            else (acc, s, inp)
      where
        safe_condition [] m = True
        safe_condition a m = condition (last a) m

p_many_while_1 :: Parser a -> (a -> a -> Bool) -> Parser [a]
p_many_while_1 p condition input =
  case run p input of
    Failure m -> Failure m
    Success (matched, string, rest) ->
      let (results, string2, rest2) = to_success (p_many_while p condition rest)
       in if should_fail matched results
            then Success ([matched], string, rest)
            else Success (matched : results, string <> string2, rest2)
  where
    should_fail a [] = False
    should_fail a (x:xs) = not $ condition a x

p_until :: forall a b. Parser a -> Parser b -> Parser ([a], Maybe b)
p_until p_a p_b = Success . p_until' [] (Just "")
  where
    run_both input = (run p_a input, run p_b input)
    p_until' ::
         [a] -> MatchedString -> Input -> (([a], Maybe b), MatchedString, Input)
    p_until' as already_matched_string input =
      case run_both input of
        (Failure _, Failure _) -> ((as, Nothing), already_matched_string, input)
        (_, Success (matched, matched_string, rest)) ->
          ((as, Just matched), already_matched_string <> matched_string, rest)
        (Success (matched, matched_string, rest), Failure _) ->
          p_until'
            (as ++ [matched])
            (already_matched_string <> matched_string)
            rest

p_until_separated ::
     forall a b c. Parser a -> Parser b -> String -> Parser ([a], Maybe b)
p_until_separated p_a p_b s = Success . p_until_separated' [] (Just "")
  where
    run_both input = (run p_a input, run p_b input)
    p_s = p_string s
    p_until_separated' ::
         [a] -> MatchedString -> Input -> (([a], Maybe b), MatchedString, Input)
    p_until_separated' as already_matched_string input =
      case run_both input of
        (Failure _, Failure _) -> ((as, Nothing), already_matched_string, input)
        (_, Success (matched, matched_string, rest)) ->
          ((as, Just matched), already_matched_string <> matched_string, rest)
        (Success (matched, matched_string, rest), Failure _) ->
          case p_s rest of
            Success (_, matched_separator_string, rest') ->
              p_until_separated'
                (as ++ [matched])
                (already_matched_string <> matched_string <>
                 matched_separator_string)
                rest'
            Failure _ ->
              ( (as ++ [matched], Nothing)
              , already_matched_string <> matched_string
              , rest)

ws :: Parser b -> Parser b
ws parser = surrounded parser "\n"

p_letter :: Parser String
p_letter = p_any_of_fast $ (: []) <$> (['a' .. 'z'] ++ ['A' .. 'Z'])

p_lowercase_letter :: Parser String
p_lowercase_letter = p_any_of_fast $ (: []) <$> ['a' .. 'z']

p_uppercase_letter :: Parser String
p_uppercase_letter = p_any_of_fast $ (: []) <$> ['A' .. 'Z']

p_digit :: Parser String
p_digit = p_any_of_fast $ (: []) <$> ['0' .. '9']

p_special :: Parser String
p_special =
  p_any_of_fast $
  (: []) <$>
  [ '.'
  , ','
  , '/'
  , '\''
  , '-'
  , '!'
  , '~'
  , '@'
  , '#'
  , '$'
  , '%'
  , '^'
  , '&'
  , '*'
  , '('
  , ')'
  , '{'
  , '}'
  , '['
  , '_'
  , '+'
  , '='
  , '|'
  , ']'
  , '<'
  , '`'
  , '>'
  , '?'
  , ':'
  , ';'
  , head "'"
  ]

symbol :: String -> Parser String
symbol string = ws $ p_string string

p_whitespace :: Parser [String]
p_whitespace = many $ p_string " " <^> p_string "\t"

p_whitespace1 :: Parser [String]
p_whitespace1 = many1 $ p_string " " <^> p_string "\t"

p_whitespace_n :: Parser [String]
p_whitespace_n = many $ p_string " " <^> p_string "\n" <^> p_string "\t"

p_whitespace1_n :: Parser [String]
p_whitespace1_n = many1 $ p_string " " <^> p_string "\n" <^> p_string "\t"

surrounded :: Parser b -> String -> Parser b
surrounded parser "[" = between parser (p_string "[", p_string "]") |>> middle
surrounded parser "{" = between parser (p_string "{", p_string "}") |>> middle
surrounded parser "(" = between parser (p_string "(", p_string ")") |>> middle
surrounded parser "%{" = between parser (p_string "%{", p_string "}") |>> middle
surrounded parser "\"" =
  between parser (p_string "\"", p_string "\"") |>> middle
surrounded parser "\"\"\"" =
  between parser (p_string "\"\"\"", p_string "\"\"\"") |>> middle
surrounded parser " " = between parser (p_whitespace, p_whitespace) |>> middle
surrounded parser "\n" =
  between parser (p_whitespace_n, p_whitespace_n) |>> middle

-- infixr 7 *...>
(*...>) :: String -> Parser b -> Parser b
(*...>) s parser = (p_string s >..> p_whitespace1) *..> parser

-- infixr 7 >...*
(>...*) :: Parser a -> String -> Parser a
(>...*) parser s = parser >..* (p_whitespace >..> p_string s)

middle :: (a, b, c) -> b
middle (_, a, _) = a

p_any :: Parser String
p_any = any_of [p_letter, p_digit, p_special]