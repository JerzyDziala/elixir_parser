{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module AST where

data NodeType
  = Program'
  | File'
  | Module'
  | ModuleLevelExpression'
  | Expression'
  | ModuleName'
  | FunctionDefinitionHead'

type Body b = [AST 'Expression' b]

type ModuleBody b = [AST ModuleLevelExpression' b]

data AST (a :: NodeType) b where
  Program
    :: { program_files :: [AST File' b]
       , program_annotation :: b}
    -> AST Program' b
  File
    :: { file_path :: String
       , file_module :: AST Module' b
       , file_annotation :: b}
    -> AST File' b
  Module
    :: { module_name :: AST ModuleName' b
       , module_body :: ModuleBody b
       , module_annotation :: b}
    -> AST Module' b
  Path
    :: { path_elements :: [String]
       , path_annotation :: b}
    -> AST ModuleName' b
  CurrentModule :: { current_module_annotation :: b} -> AST ModuleName' b
  ModuleConstant
    :: { module_constant_name :: String
       , module_constant_value :: AST Expression' b
       , module_constant_annotation :: b}
    -> AST ModuleLevelExpression' b
  Alias
    :: { alias_name :: AST ModuleName' b
       , alias_annotation :: b}
    -> AST ModuleLevelExpression' b
  ModuleLevelExpression
    :: { module_level_expression_expression :: AST Expression' b
       , module_level_expression_annotation :: b}
    -> AST ModuleLevelExpression' b
  SchemaDefinition
    :: { schema_definition_name :: String
       , schema_definition_body :: Body b
       , schema_definition_annotation :: b}
    -> AST ModuleLevelExpression' b
  FieldAccess
    :: { field_access_left_side :: AST Expression' b
       , field_access_name :: String
       , field_access_annotation :: b}
    -> AST Expression' b
  StringLiteral
    :: { string_literal_value :: String
       , string_literal_is_multiline :: Bool
       , string_literal_annotation :: b}
    -> AST Expression' b
  NumberLiteral
    :: { number_literal_value :: Int
       , number_literal_annotation :: b}
    -> AST Expression' b
  NilLiteral :: { nil_literal_annotation :: b} -> AST Expression' b
  BoolLiteral
    :: { bool_literal_value :: Bool
       , bool_literal_annotation :: b}
    -> AST Expression' b
  AtomLiteral
    :: { atom_literal_value :: String
       , atom_literal_annotation :: b}
    -> AST Expression' b
  ListLiteral
    :: { list_literal_items :: Body b
       , list_literal_annotation :: b}
    -> AST Expression' b
  TupleLiteral
    :: { tuple_literal_items :: Body b
       , tuple_literal_annotation :: b}
    -> AST Expression' b
  Capture
    :: { capture_expression :: AST Expression' b
       , capture_annotation :: b}
    -> AST Expression' b
  Sigil
    :: { sigil_starting_character :: String
       , sigil_left_char :: String
       , sigil_right_char :: String
       , sigil_content :: String
       , sigil_modifier :: Maybe String
       , sigil_annotation :: b}
    -> AST Expression' b
  KeywordListLiteral
    :: { keyword_list_literal_items :: [(AST Expression' b, AST Expression' b)]
       , keyword_list_literal_is_argument_list :: Bool
       , keyword_list_literal_annotation :: b}
    -> AST Expression' b
  StructLiteral
    :: { struct_literal_name :: AST ModuleName' b
       , struct_literal_items :: [(AST Expression' b, AST Expression' b)]
       , struct_literal_annotation :: b}
    -> AST Expression' b
  HashLiteral
    :: { hash_literal_items :: [(AST Expression' b, AST Expression' b)]
       , hash_literal_annotation :: b}
    -> AST Expression' b
  ModuleName
    :: { module_name_module_name :: AST ModuleName' b
       , module_name_annotation :: b}
    -> AST Expression' b
  BoolNegation
    :: { bool_negation_expression :: AST Expression' b
       , bool_negation_annotation :: b}
    -> AST Expression' b
  Parenthesis
    :: { parenthesis_expression :: AST Expression' b
       , parenthesis_annotation :: b}
    -> AST Expression' b
  Tick
    :: { tick_expression :: AST Expression' b
       , tick_annotation :: b}
    -> AST Expression' b
  InfixOperator
    :: { infix_operator_left_side :: AST Expression' b
       , infix_operator_right_side :: AST Expression' b
       , infix_operator :: String
       , infix_operator_annotation :: b}
    -> AST Expression' b
  StructUpdate
    :: { struct_update_name :: AST ModuleName' b
       , struct_update_struct :: AST Expression' b
       , struct_update_body :: [(AST Expression' b, AST Expression' b)]
       , struct_update_annotation :: b}
    -> AST Expression' b
  Lambda
    :: { lambda_cases :: [([AST Expression' b], [AST Expression' b])]
       , lambda_annotation :: b}
    -> AST Expression' b
  FunctionApplication
    :: { function_application_module_name :: Maybe (AST ModuleName' b)
       , function_application_name :: String
       , function_application_arguments :: Body b
       , function_application_body :: Maybe (Body b)
       , is_parenthesised :: Bool
       , function_application_annotation :: b}
    -> AST Expression' b
  FunctionApplicationOrVariable
    :: { function_application_or_variable_name :: String
       , function_application_or_variable_annotation :: b}
    -> AST Expression' b
  ModuleConstantReference
    :: { module_constant_reference_name :: String
       , module_constant_reference_annotation :: b}
    -> AST Expression' b
  BindOperator
    :: { bind_left_side :: AST Expression' b
       , bind_right_side :: AST Expression' b
       , bind_operator_annotation :: b}
    -> AST Expression' b
  Case
    :: { case_expression :: AST Expression' b
       , case_cases :: [(AST Expression' b, AST Expression' b)]
       , case_annotation :: b}
    -> AST Expression' b
  If
    :: { if_condition :: AST Expression' b
       , if_body :: Body b
       , if_else_body :: Maybe (Body b)
       , if_annotation :: b}
    -> AST Expression' b
  With
    :: { with_matches :: [(AST Expression' b, AST Expression' b)]
       , with_body :: Body b
       , with_else_body :: Maybe (Body b)
       , with_annotation :: b}
    -> AST Expression' b
  FunctionDefinitionGroup :: {
    function_definition_group_heads :: [AST FunctionDefinitionHead' b],
    function_definition_group_annotation :: b
  } -> AST ModuleLevelExpression' b
  FunctionDefinitionHead
    :: { function_definition_head_spec :: Maybe (AST ModuleLevelExpression' b)
       , function_definition_head_keyword :: String
       , function_definition_head_name :: String
       , function_definition_head_parameters :: [AST Expression' b]
       , function_definition_head_guard :: Maybe (AST Expression' b)
       , function_definition_head_body :: Maybe (Body b)
       , function_definition_head_annotation :: b
   } -> AST FunctionDefinitionHead' b

deriving instance Show b => Show (AST a b)

deriving instance Ord b => Ord (AST a b)

deriving instance Eq b => Eq (AST a b)