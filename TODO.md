# TODO

## IDE

- make source fully tokenizable so that each token maps to source and all token
  length = source length
- treat bad tokens as trivia
- get location spans from whole syntax nodes
- centralize error reporting
- don't exit on error

## Language features that are enough like C++ to compile as C++

- implement generics
- think about if we want to implement function overloading at all. maybe we
  actually don't want that and instead can try to make function calls more
  convenient with named argument passing of optional parameters.
  - we might need to implement a restricted version to support generics.
  - we need to support a full version if we want sane UFC instead of class
    methods
- implement operator overloading
- implement auto
- optional parameters
- struct initializer / default constructors
- maybe class methods
- introduce concept of truthyness for logical operators || && and conditions
  like in if, while, for
- think about the order we want to implement stuff for the language. especially
  stuff that forces us to depart from the c/c++ syntax. if we do this step we
  should have some alternative ready. at the very least syntax highlighting
  would be a must. either through vscode or through a custom editor (maybe this
  is fun? https://viewsourcecode.org/snaptoken/kilo/)

## Debugging / Testing

- dumping AST and SymbolTable
- test extern things more thorougly (structs/unions, enums, functions and also
  global variables)

## Misc

- implement 'using' typedef thingy if necessary
  (https://github.com/DoctorWkt/acwj/tree/master/34_Enums_and_Typedefs)
- think about array size initialization and constant folding and whether we need
  this or not. currently we cannot do let int SOME_CONSTANT = 128; let int
  arr[SOME_CONSTANT + 4];
  (https://github.com/DoctorWkt/acwj/tree/master/44_Fold_Optimisation)
  (https://github.com/DoctorWkt/acwj/blob/master/45_Globals_Again/Readme.md) we
  can also go the route to just always heap alloc our arrays and make them
  stretchy

---

---

# DONE

- token create empty vs token create missing
- slowly change the architecture to scan and parse files on their own
  - first make the grammar of the language such that we know what a
    statement/expression is without a symbol or type table (like we did in the
    TS compiler)
  - for this we need to split AST into pure syntax tree and a bound tree (like
    in TS compiler)
  - scan and parse files on their own and look for import statements (like in
    the TS compiler)
  - get rid of the 'Source' thing with its details and file inclusion x make let
    and letpersist keywords instead of let + localpersist
- introduce trivia in tokens
- get rid of all the parentheses in the output. we could use the fact that our
  precedences match that of C itself. therefore we could make parenthesized
  expression explicit in the AST/emitter and only use parentheses for
  ASTKind::ParenthesizesExpression. We would need to be careful though with the
  type and lvalueness of such an expression. So we need to make sure that
  postfix operators like [] and -> still work for parenthesized expressions x -
  get rid of the 'include' file concept. for this we need:
