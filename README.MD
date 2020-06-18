This is a part of a bigger program that was used to analyse and transform
a big number of ecto schemas in a large elixir codebase.

you can play with it in repl using the command: stack repl

then you can use the parsing functions to turn strings into elixir ASTs

examples:

    parse_expression "SomeModule.some_function 3, 4 + 1"
    
    parse_module_level_expression "@some_attribute \"some string\""
    
    parse_module "defmodule SomeModule do\n  alias Some.Other.Module\nend"

