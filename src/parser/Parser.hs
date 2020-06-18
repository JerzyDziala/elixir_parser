{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Parser where

import AST

import Annotations
import Combinators
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Bifunctor
import Utils
import Result

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


parse_module :: String -> Either (AST 'Module' LexicalInfo) String
parse_module = parse p_module

parse_expression :: String -> Either (AST 'Expression' LexicalInfo) String
parse_expression = parse p_expression

parse_module_level_expression :: String -> Either (AST 'ModuleLevelExpression' LexicalInfo) String
parse_module_level_expression = parse p_module_level_expression

parse :: Parser a -> String ->  Either a String
parse parser input =
  case run parser (input, (0,0)) of
    (Success (result, _, _)) -> Left result
    (Failure message) -> Right message


data ExpressionType
  = SimpleLiteralExpression
  | ApplicationOrVariableExpression
  | ModuleNameExpression
  | InfixOperatorExpression
  | LambdaExpression
  | BindOperatorExpression
  | KeywordListLiteralExpression
  | StructUpdateExpression
  | TupleLiteralExpression
  | HashLiteralExpression
  | SigilExpression
  | NilLiteralExpression
  | StructLiteralExpression
  | ListLiteralExpression
  | ParenthesisExpression
  | BoolNegationExpression
  | NonParenthesisedFunctionExpression
  | ParenthesisedFunctionApplicationExpression
  | WithExpression
  | IfExpression
  | CaseExpression
  | ModuleConstantReferenceExpression
  | TickExpression
  | CaptureExpression
  | FieldAccessExpression
  deriving (Eq)

all_expressions :: [(Parser (AST Expression' LexicalInfo), ExpressionType)]
all_expressions =
  [(p_simple_literal, SimpleLiteralExpression),
   (p_sigil, SigilExpression),
   (p_bool_negation, BoolNegationExpression),
   (p_infix_operator, InfixOperatorExpression),
   (p_lambda, LambdaExpression),
   (p_non_parenthesized_function, NonParenthesisedFunctionExpression),
   (p_parenthesized_function,
    ParenthesisedFunctionApplicationExpression),
   (p_application_or_variable, ApplicationOrVariableExpression),
   (p_struct_update, StructUpdateExpression),
   (p_module_name_expression, ModuleNameExpression),
   (p_bind_operator, BindOperatorExpression),
   (p_keyword_list_literal, KeywordListLiteralExpression),
   (p_nil_literal, NilLiteralExpression),
   (p_tuple_literal, TupleLiteralExpression),
   (p_hash_literal, HashLiteralExpression),
   (p_struct_literal, StructLiteralExpression),
   (p_list_literal, ListLiteralExpression),
   (p_parenthesis, ParenthesisExpression),
   (p_with_expression, WithExpression),
   (p_if_expression, IfExpression),
   (p_case_expression, CaseExpression),
   (p_tick_expression, TickExpression),
   (p_module_constant_reference, ModuleConstantReferenceExpression),
   (p_field_access, FieldAccessExpression),
   (p_capture, CaptureExpression)]
    <$$> first ws

p_expression :: Parser (AST Expression' LexicalInfo)
p_expression = any_of (fst <$> all_expressions)

p_expression_without_non_parenthesised_functions =
  any_of $
  fst <$>
  subtract_by
    snd
    all_expressions
    [(p_non_parenthesized_function, NonParenthesisedFunctionExpression)]

p_expression_without_left_recursive_expressions ::
     Parser (AST Expression' LexicalInfo)
p_expression_without_left_recursive_expressions =
  any_of $ fst <$> subtract_by snd all_expressions [ (p_infix_operator, InfixOperatorExpression)
                                                     , (p_bind_operator, BindOperatorExpression)
                                                     ]

p_expression_without_left_recursive_expressions_for_field_access ::
     Parser (AST Expression' LexicalInfo)
p_expression_without_left_recursive_expressions_for_field_access =
  any_of $
  fst <$> subtract_by snd all_expressions [ (p_infix_operator, InfixOperatorExpression)
                                            , (p_bind_operator, BindOperatorExpression)
                                            , (p_field_access, FieldAccessExpression)
                                            ]

p_schema_definition :: Parser (AST ModuleLevelExpression' LexicalInfo)
p_schema_definition =
  (p_string "schema" *..> p_string_literal) >..> p_do_block
    |>> first string_literal_value |>>>
  uncurry SchemaDefinition

p_alias :: Parser (AST 'ModuleLevelExpression' LexicalInfo)
p_alias = p_string "alias" *..> p_module_name |>>> Alias

p_application_or_variable :: Parser (AST Expression' LexicalInfo)
p_application_or_variable = p_identifier |>>> FunctionApplicationOrVariable

p_function_application :: Parser (AST 'Expression' LexicalInfo)
p_function_application =
  p_non_parenthesized_function <|> p_parenthesized_function

p_non_parenthesized_function :: Parser (AST 'Expression' LexicalInfo)
p_non_parenthesized_function =
  p_function_name >..> (p_whitespace1_n *..> p_argument_list_body) >..>
  optional p_do_block |>>>
  create_non_parenthesised_function_application
  where
    create_non_parenthesised_function_application (((module_name, name), arguments), block) =
      FunctionApplication module_name name arguments block False

p_parenthesized_function :: Parser (AST 'Expression' LexicalInfo)
p_parenthesized_function =
  p_function_name >..> surrounded p_argument_list_body "(" |>>>
  create_parenthesised_function_application
  where
    create_parenthesised_function_application ((module_name, name), arguments) =
      FunctionApplication module_name name arguments Nothing True

p_function_name :: Parser (Maybe (AST 'ModuleName' LexicalInfo), String)
p_function_name = optional (p_module_name >..* p_string ".") >..> p_identifier

p_sigil :: Parser (AST 'Expression' LexicalInfo)
p_sigil =
  p_string "~" *..> p_letter >..> (p_sigil_body' >..> optional p_letter) |>>>
  create_sigil
  where
    create_sigil (letter, ((left, (body, right)), modifier)) =
      Sigil letter left (fromJust right) (join body) modifier
    p_sigil_body' =
      p_sigil_body "(" ")" <|> p_sigil_body "[" "]" <|> p_sigil_body "/" "/" <|>
      p_sigil_body "|" "|" <|>
      p_sigil_body "\"" "\"" <|>
      p_sigil_body "'" "'" <|>
      p_sigil_body "{" "}" <|>
      p_sigil_body "<" ">"
    p_sigil_body a b =
      (p_string a >..>
       p_until (p_any <|> p_string " " <|> p_string "\n") (p_string b)) <!>
      (\((left, (body, right)), _, _) ->
         case right of
           Nothing -> True
           _ -> False)

p_do_block :: Parser [AST Expression' LexicalInfo]
p_do_block = p_string "do" *..> many p_expression >..* p_string "end"

p_argument_list_body :: Parser [AST Expression' LexicalInfo]
p_argument_list_body =
  p_until_separated
    p_expression
    (p_keyword_list_body1 |>>> \items -> KeywordListLiteral items True)
    "," |>> \(es, k) -> maybe es ((es ++) . return) k

p_infix_operator :: Parser (AST Expression' LexicalInfo)
p_infix_operator =
  p_expression_without_left_recursive_expressions >..>
  (p_operator >..> p_expression) |>>>
  create_infix_operator
  where
    p_operator =
      p_any_of
        [ "+"
        , "-"
        , "*"
        , "/"
        , "|"
        , "&&"
        , "||"
        , "::"
        , "|>"
        , ">"
        , "<"
        , "=="
        , "!="
        , ">="
        , "<="
        , ".."
        , "<>"
        , "++"
        , "--"
        , "\\\\"
        , "and"
        , "in"
        , "or"
        ]
    create_infix_operator (a, (o, b)) = InfixOperator a b o

p_tick_expression :: Parser (AST Expression' LexicalInfo)
p_tick_expression = p_string "^" *..> p_expression |>>> Tick

p_parenthesis :: Parser (AST Expression' LexicalInfo)
p_parenthesis = surrounded p_expression "(" |>>> Parenthesis

p_capture :: Parser (AST Expression' LexicalInfo)
p_capture = p_string "&" *..> p_expression |>>> Capture

p_bool_negation :: Parser (AST Expression' LexicalInfo)
p_bool_negation = (p_string "!" *..> p_expression') |>>> BoolNegation
  where
    p_expression' =
      any_of $
      fst <$>
      subtract_by
        snd
        all_expressions
        [(p_infix_operator, InfixOperatorExpression)]

p_module_constant_reference :: Parser (AST Expression' LexicalInfo)
p_module_constant_reference =
  p_string "@" *..> p_identifier |>>> ModuleConstantReference

p_list_literal :: Parser (AST Expression' LexicalInfo)
p_list_literal = surrounded p_expressions_list "[" |>>> ListLiteral

p_expressions_list :: Parser [AST Expression' LexicalInfo]
p_expressions_list = many_separated p_expression ","

p_hash_literal :: Parser (AST Expression' LexicalInfo)
p_hash_literal = surrounded p_hash_body "%{" |>>> HashLiteral

p_hash_body :: Parser [(AST 'Expression' LexicalInfo, AST 'Expression' LexicalInfo)]
p_hash_body =
  p_until_separated p_hash_pair p_keyword_list_body1 "," |>> \(a, b) ->
    maybe a (a ++) b
  where
    p_hash_pair = (p_expression >..* p_string "=>") >..> p_expression

p_struct_literal :: Parser (AST Expression' LexicalInfo)
p_struct_literal =
  (p_string "%" *..> p_module_name >..* p_string "{") >..> ws p_hash_body >..*
  symbol "}" |>>>
  uncurry StructLiteral

p_tuple_literal :: Parser (AST Expression' LexicalInfo)
p_tuple_literal = surrounded p_expressions_list "{" |>>> TupleLiteral

p_keyword_list_literal :: Parser (AST Expression' LexicalInfo)
p_keyword_list_literal =
  surrounded (p_keyword_list_body1 |>>> (`KeywordListLiteral` False)) "["

p_keyword_list_body1 ::
     Parser [(AST Expression' LexicalInfo, AST Expression' LexicalInfo)]
p_keyword_list_body1 = many1_separated p_atom_key_value_pair ","

p_keyword_list_body ::
     Parser [(AST Expression' LexicalInfo, AST Expression' LexicalInfo)]
p_keyword_list_body = many_separated p_atom_key_value_pair ","

p_atom_key_value_pair ::
     Parser (AST Expression' LexicalInfo, AST Expression' LexicalInfo)
p_atom_key_value_pair = p_atom >..> p_expression
  where
    p_atom = p_identifier_allow_keywords >..* p_string ":" |>>> AtomLiteral

p_bind_operator :: Parser (AST Expression' LexicalInfo)
p_bind_operator =
  (p_expression_without_left_recursive_expressions >..* p_string "=") >..>
  p_expression |>>>
  uncurry BindOperator

p_if_expression :: Parser (AST Expression' LexicalInfo)
p_if_expression = p_if_expression' |>>> create_if_expression
  where
    p_if_expression' =
      p_string "if" *..> (p_expression >..* p_string "do") >..>
      many p_expression >..>
      optional p_else >..*
      p_string "end"
    p_else = p_string "else" *..> many p_expression
    create_if_expression ((condition, body), else_body) =
      If condition body else_body

p_with_expression :: Parser (AST Expression' LexicalInfo)
p_with_expression = p_with_Expression' |>>> create_with_expression
  where
    p_with_Expression' =
      (p_string "with" *..> p_matches >..* p_string "do") >..> many p_expression >..>
      optional p_else >..*
      p_string "end"
    p_else = p_string "else" *..> many p_expression
    p_matches =
      many1_separated ((p_expression >..* p_string "<-") >..> p_expression) ","
    create_with_expression ((matches, body), else_body) =
      With matches body else_body

p_case_expression :: Parser (AST Expression' LexicalInfo)
p_case_expression = p_case_expression' |>>> uncurry Case
  where
    p_case_expression' =
      (p_string "case" *..> p_expression >..* p_string "do") >..> many1 p_case >..*
      p_string "end"
    p_case =
      (p_expression >..* p_string "->") >..>
      p_expression_without_non_parenthesised_functions

p_field_access :: Parser (AST Expression' LexicalInfo)
p_field_access =
  p_expression_without_left_recursive_expressions_for_field_access >..>
  many1 (p_string "." *..> p_identifier) |>>>
  create_field_access
  where
    create_field_access (parent, [field]) = FieldAccess parent field
    create_field_access (parent, fields) =
      FieldAccess
        (create_field_access (parent, init fields) empty_lexical_info)
        (last fields)

p_module_name_expression :: Parser (AST Expression' LexicalInfo)
p_module_name_expression = p_module_name |>>> ModuleName

p_module_name :: Parser (AST ModuleName' LexicalInfo)
p_module_name = path <|> current
  where
    path = many1_separated p_uppercase_identifier "." |>>> Path
    current = p_string "__MODULE__" |>>> const CurrentModule

p_module :: Parser (AST Module' LexicalInfo)
p_module =
  (p_string "defmodule" *..> p_module_name >..* p_string "do") >..>
  (many p_module_level_expression >..* p_string "end") |>>>
  uncurry Module

p_module_level_expression :: Parser (AST ModuleLevelExpression' LexicalInfo)
p_module_level_expression =
  p_schema_definition <|> p_module_constant <|> p_function_definition_group <|>
  (p_expression |>>> ModuleLevelExpression) <|>
  p_alias

p_struct_update :: Parser (AST Expression' LexicalInfo)
p_struct_update =
  (p_string "%" *..> p_module_name >..* p_string "{") >..>
  (p_expression_without_left_recursive_expressions >..* p_string "|") >..>
  p_hash_body >..*
  p_string "}" |>>>
  create_struct_update
  where
    create_struct_update ((name, expression), body) =
      StructUpdate name expression body

p_module_constant :: Parser (AST ModuleLevelExpression' LexicalInfo)
p_module_constant =
  ws $
  (p_string "@" *..> p_identifier) >..> p_expression |>>> uncurry ModuleConstant

p_lambda :: Parser (AST 'Expression' LexicalInfo)
p_lambda =
  p_string "fn" *..> many1 p_single_lambda >..* symbol "end" |>>> Lambda
  where
    p_single_lambda =
      (p_argument_list_body >..* p_string "->") >..> many1 p_expression

p_function_definition_group :: Parser (AST 'ModuleLevelExpression' LexicalInfo)
p_function_definition_group =
  p_many_while_1
    p_function_definition_head
    (\a b -> function_definition_head_name a == function_definition_head_name b) |>>>
  FunctionDefinitionGroup

p_function_definition_head :: Parser (AST 'FunctionDefinitionHead' LexicalInfo)
p_function_definition_head =
  p_function_definition' |>>> create_function_definition <!>
  (\(definition, _, _) ->
     ((/= "spec") . module_constant_name <$>
      function_definition_head_spec definition) ??
     False)
  where
    create_function_definition (((((spec, keyword), name), params), guard), body) =
      FunctionDefinitionHead spec keyword name (fold params) guard body
    p_function_definition' =
      optional p_module_constant >..> p_def >..>
      (p_whitespace1 *..> p_identifier) >..>
      optional (surrounded p_argument_list_body "(") >..>
      optional (p_whitespace1_n *..> p_guard) >..>
      optional
        ((p_whitespace *..> p_do_block) <|> p_single_expression_definition_body)
    p_single_expression_definition_body =
      (p_string "," >..> (p_whitespace *..> p_string "do:")) *..> p_expression |>>
      (: [])
    p_def =
      p_string "def" <|> p_string "defp" <|> p_string "defmacro" <|>
      p_string "defmacrop"

p_guard :: Parser (AST Expression' LexicalInfo)
p_guard = p_string "when" *..> p_expression

p_nil_literal :: Parser (AST Expression' LexicalInfo)
p_nil_literal = p_string "nil" |>>> const NilLiteral

p_number_literal :: Parser (AST Expression' LexicalInfo)
p_number_literal =
  optional (p_string "-") >..> p_number |>>>
  (\(sign, number) -> NumberLiteral (read (fold sign ++ number) :: Int))
  where
    p_number =
      (any_of [p_binary, p_octal, p_hex] |>> (\(a, b) -> a ++ concat b)) <|>
      (many1 (p_digit <|> p_string "_") |>> concat)
    p_binary = p_string "0b" >..> many1 p_0_or_1
    p_octal = p_string "0o" >..> many1 p_from_0_to_7
    p_hex = p_string "0x" >..> many1 p_hex_digit
    p_from_0_to_7 = any_of $ map (p_string . (: [])) ['0' .. '7']
    p_0_or_1 = p_string "0" <|> p_string "1"
    p_hex_digit =
      any_of $ map (p_string . (: [])) $ ['0' .. '9'] ++ ['A' .. 'F']

p_string_literal :: Parser (AST Expression' LexicalInfo)
p_string_literal =
  (p_multi_line_string |>>> (`StringLiteral` True)) <|>
  (p_single_line_string |>>> (`StringLiteral` False))

p_string' :: Parser String
p_string' = p_multi_line_string <|> p_single_line_string

p_single_line_string :: Parser String
p_single_line_string = surrounded p_string_body "\""

p_multi_line_string :: Parser String
p_multi_line_string = surrounded p_multiline_string_body "\"\"\""

p_string_body :: Parser String
p_string_body = many (p_any <|> p_string " " <|> p_string "\t" <|> p_string "\n") |>> concat

p_multiline_string_body :: Parser String
p_multiline_string_body =
  many (p_any <|> p_string " " <|> p_string "\n" <|> p_string "\t") |>> concat

p_bool_literal :: Parser (AST Expression' LexicalInfo)
p_bool_literal = p_bool |>>> (BoolLiteral . to_bool)
  where
    p_bool = p_string "true" <|> p_string "false"
    to_bool "true" = True
    to_bool "false" = False

p_atom_literal :: Parser (AST Expression' LexicalInfo)
p_atom_literal = p_atom |>>> AtomLiteral
  where
    p_atom = p_string ":" *..> p_identifier_allow_keywords

p_simple_literal :: Parser (AST Expression' LexicalInfo)
p_simple_literal =
  p_string_literal <|> p_bool_literal <|> p_atom_literal <|> p_number_literal

p_identifier_allow_keywords :: Parser String
p_identifier_allow_keywords = p_identifier'
  where
    p_identifier' =
      (p_lowercase_letter <^> p__) >..>
      (many (any_of [p_letter, p_digit, p__]) >..>
       optional (p_any_of_fast ["?", "!"])) |>>
      combine <?>
      "identifier"
    combine (a, (b, c)) = a ++ concat b ++ fold c
    p__ = p_string "_"

p_identifier :: Parser String
p_identifier = p_identifier' <!> \(matched, _, _) -> matched `elem` keywords
  where
    p_identifier' =
      (p_lowercase_letter <^> p__) >..>
      (many (any_of [p_letter, p_digit, p__]) >..>
       optional (p_any_of_fast ["?", "!"])) |>>
      combine <?>
      "identifier"
    combine (a, (b, c)) = a ++ concat b ++ fold c
    p__ = p_string "_"
    keywords =
      [ "do"
      , "end"
      , "if"
      , "else"
      , "case"
      , "cond"
      , "true"
      , "nil"
      , "false"
      , "with"
      , "or"
      , "and"
      , "schema"
      , "defmodule"
      , "def"
      , "defp"
      , "defmacro"
      , "defmacrop"
      ]

p_uppercase_identifier :: Parser String
p_uppercase_identifier = p_uppercase_identifier' <?> "uppercase identifier"
  where
    p_uppercase_identifier' =
      p_uppercase_letter >..> many (any_of [p_letter, p_digit, p__]) |>> combine
    combine (a, b) = a ++ concat b
    p__ = p_string "_"
