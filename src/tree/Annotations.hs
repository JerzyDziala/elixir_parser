{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

module Annotations where

import AST

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

class GenerationReadyAnnotation a where
  get_left_space :: a -> String
  get_right_space :: a -> String
  get_original_string :: a -> Maybe String
  without_original_string :: a -> a

data LexicalInfo =
  LexicalInfo
    { original_string :: Maybe String
    , left_space :: String
    , right_space :: String
    , original_file :: Maybe (AST File' LexicalInfo)
    }
  deriving (Show, Ord)

instance GenerationReadyAnnotation LexicalInfo where
  get_left_space = left_space
  get_right_space = right_space
  get_original_string = original_string
  without_original_string a = a {original_string = Nothing}

empty_lexical_info =
  LexicalInfo
    { original_string = Nothing
    , left_space = ""
    , right_space = ""
    , original_file = Nothing
    }

instance Eq LexicalInfo where
  a == b = True

get_annotation :: AST a b -> b
get_annotation Program {program_annotation} = program_annotation
get_annotation File {file_annotation} = file_annotation
get_annotation Module {module_annotation} = module_annotation
get_annotation Path {path_annotation} = path_annotation
get_annotation Tick {tick_annotation} = tick_annotation
get_annotation CurrentModule {current_module_annotation} =
  current_module_annotation
get_annotation ModuleConstant {module_constant_annotation} =
  module_constant_annotation
get_annotation Alias {alias_annotation} = alias_annotation
get_annotation ModuleLevelExpression {module_level_expression_annotation} =
  module_level_expression_annotation
get_annotation SchemaDefinition {schema_definition_annotation} =
  schema_definition_annotation
get_annotation ListLiteral {list_literal_annotation} = list_literal_annotation
get_annotation TupleLiteral {tuple_literal_annotation} =
  tuple_literal_annotation
get_annotation KeywordListLiteral {keyword_list_literal_annotation} =
  keyword_list_literal_annotation
get_annotation StructLiteral {struct_literal_annotation} =
  struct_literal_annotation
get_annotation StructUpdate {struct_update_annotation} =
  struct_update_annotation
get_annotation HashLiteral {hash_literal_annotation} = hash_literal_annotation
get_annotation Sigil {sigil_annotation} = sigil_annotation
get_annotation StringLiteral {string_literal_annotation} =
  string_literal_annotation
get_annotation NilLiteral {nil_literal_annotation} = nil_literal_annotation
get_annotation NumberLiteral {number_literal_annotation} =
  number_literal_annotation
get_annotation BoolLiteral {bool_literal_annotation} = bool_literal_annotation
get_annotation AtomLiteral {atom_literal_annotation} = atom_literal_annotation
get_annotation FunctionApplicationOrVariable {function_application_or_variable_annotation} =
  function_application_or_variable_annotation
get_annotation FieldAccess {field_access_annotation} = field_access_annotation
get_annotation ModuleName {module_name_annotation} = module_name_annotation
get_annotation BoolNegation {bool_negation_annotation} =
  bool_negation_annotation
get_annotation Parenthesis {parenthesis_annotation} = parenthesis_annotation
get_annotation Lambda {lambda_annotation} = lambda_annotation
get_annotation Capture {capture_annotation} = capture_annotation
get_annotation ModuleConstantReference {module_constant_reference_annotation} =
  module_constant_reference_annotation
get_annotation InfixOperator {infix_operator_annotation} =
  infix_operator_annotation
get_annotation FunctionApplication {function_application_annotation} =
  function_application_annotation
get_annotation BindOperator {bind_operator_annotation} =
  bind_operator_annotation
get_annotation Case {case_annotation} = case_annotation
get_annotation If {if_annotation} = if_annotation
get_annotation With {with_annotation} = with_annotation
get_annotation FunctionDefinitionGroup {function_definition_group_annotation} =
  function_definition_group_annotation
get_annotation FunctionDefinitionHead {function_definition_head_annotation} =
  function_definition_head_annotation

with_no_string info = info {original_string = Nothing}

map_annotation :: (b -> b) -> AST a b -> AST a b
map_annotation mapping node@Program {program_annotation} =
  node {program_annotation = mapping program_annotation}
map_annotation mapping node@File {file_annotation} =
  node {file_annotation = mapping file_annotation}
map_annotation mapping node@Module {module_annotation} =
  node {module_annotation = mapping module_annotation}
map_annotation mapping node@Path {path_annotation} =
  node {path_annotation = mapping path_annotation}
map_annotation mapping node@CurrentModule {current_module_annotation} =
  node {current_module_annotation = mapping current_module_annotation}
map_annotation mapping node@ModuleConstant {module_constant_annotation} =
  node {module_constant_annotation = mapping module_constant_annotation}
map_annotation mapping node@Alias {alias_annotation} =
  node {alias_annotation = mapping alias_annotation}
map_annotation mapping node@ModuleLevelExpression {module_level_expression_annotation} =
  node
    { module_level_expression_annotation =
        mapping module_level_expression_annotation
    }
map_annotation mapping node@SchemaDefinition {schema_definition_annotation} =
  node {schema_definition_annotation = mapping schema_definition_annotation}
map_annotation mapping node@ListLiteral {list_literal_annotation} =
  node {list_literal_annotation = mapping list_literal_annotation}
map_annotation mapping node@TupleLiteral {tuple_literal_annotation} =
  node {tuple_literal_annotation = mapping tuple_literal_annotation}
map_annotation mapping node@KeywordListLiteral {keyword_list_literal_annotation} =
  node
    {keyword_list_literal_annotation = mapping keyword_list_literal_annotation}
map_annotation mapping node@StructLiteral {struct_literal_annotation} =
  node {struct_literal_annotation = mapping struct_literal_annotation}
map_annotation mapping node@StructUpdate {struct_update_annotation} =
  node {struct_update_annotation = mapping struct_update_annotation}
map_annotation mapping node@HashLiteral {hash_literal_annotation} =
  node {hash_literal_annotation = mapping hash_literal_annotation}
map_annotation mapping node@Sigil {sigil_annotation} =
  node {sigil_annotation = mapping sigil_annotation}
map_annotation mapping node@StringLiteral {string_literal_annotation} =
  node {string_literal_annotation = mapping string_literal_annotation}
map_annotation mapping node@NilLiteral {nil_literal_annotation} =
  node {nil_literal_annotation = mapping nil_literal_annotation}
map_annotation mapping node@NumberLiteral {number_literal_annotation} =
  node {number_literal_annotation = mapping number_literal_annotation}
map_annotation mapping node@BoolLiteral {bool_literal_annotation} =
  node {bool_literal_annotation = mapping bool_literal_annotation}
map_annotation mapping node@AtomLiteral {atom_literal_annotation} =
  node {atom_literal_annotation = mapping atom_literal_annotation}
map_annotation mapping node@FunctionApplicationOrVariable {function_application_or_variable_annotation} =
  node
    { function_application_or_variable_annotation =
        mapping function_application_or_variable_annotation
    }
map_annotation mapping node@FieldAccess {field_access_annotation} =
  node {field_access_annotation = mapping field_access_annotation}
map_annotation mapping node@ModuleName {module_name_annotation} =
  node {module_name_annotation = mapping module_name_annotation}
map_annotation mapping node@BoolNegation {bool_negation_annotation} =
  node {bool_negation_annotation = mapping bool_negation_annotation}
map_annotation mapping node@Parenthesis {parenthesis_annotation} =
  node {parenthesis_annotation = mapping parenthesis_annotation}
map_annotation mapping node@Lambda {lambda_annotation} =
  node {lambda_annotation = mapping lambda_annotation}
map_annotation mapping node@Capture {capture_annotation} =
  node {capture_annotation = mapping capture_annotation}
map_annotation mapping node@ModuleConstantReference {module_constant_reference_annotation} =
  node
    { module_constant_reference_annotation =
        mapping module_constant_reference_annotation
    }
map_annotation mapping node@InfixOperator {infix_operator_annotation} =
  node {infix_operator_annotation = mapping infix_operator_annotation}
map_annotation mapping node@FunctionApplication {function_application_annotation} =
  node
    {function_application_annotation = mapping function_application_annotation}
map_annotation mapping node@BindOperator {bind_operator_annotation} =
  node {bind_operator_annotation = mapping bind_operator_annotation}
map_annotation mapping node@Case {case_annotation} =
  node {case_annotation = mapping case_annotation}
map_annotation mapping node@If {if_annotation} =
  node {if_annotation = mapping if_annotation}
map_annotation mapping node@With {with_annotation} =
  node {with_annotation = mapping with_annotation}
map_annotation mapping node@FunctionDefinitionHead {function_definition_head_annotation} =
  node {function_definition_head_annotation = mapping function_definition_head_annotation}
map_annotation mapping node@FunctionDefinitionGroup {function_definition_group_annotation} =
  node {function_definition_group_annotation = mapping function_definition_group_annotation}
map_annotation mapping node@Tick {tick_annotation} =
  node {tick_annotation = mapping tick_annotation}