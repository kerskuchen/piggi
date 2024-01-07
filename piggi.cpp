#include "parser.cpp"
#include "emitter.cpp"
#include "preprocessor.cpp"
#include "binder.cpp"

/*
TODO:

* test extern things more thorougly (structs/unions, enums, functions and also global variables)
* introduce concept of truthyness for logical operators || && and conditions like in if, while, for
* debugging facilities like dumping AST and SymbolTable
* think about the order we want to implement stuff for the language. especially stuff that
  forces us to depart from the c/c += 1 syntax. if we do this step we should have some alternative
  ready. at the very least syntax highlighting would be a must. either through vscode or through
  a custom editor (maybe this is fun? https://viewsourcecode.org/snaptoken/kilo/)
* add some features that look enough like C += 1 that we can compile it as C += 1
    - implement function overloading
    - implement operator overloading
    - implement generics 
* - get rid of the 'include' file concept. for this we need:
  - slowly change the architecture to scan and parse files on their own
  - first make the grammar of the language such that we know what a statement/expression is without 
    a symbol or type table (like we did in the TS compiler)
  - for this we need to split AST into pure syntax tree and a bound tree (like in TS compiler)
  - scan and parse files on their own and look for import statements (like in the TS compiler)
  - get rid of the 'Source' thing with its details and file inclusion
* centralize error reporting
* introduce trivia in tokens
* make source fully tokenizable so that each token maps to source and all token lengt = source length
* improve error reporting by introducing source spans
* implement 'using' typedef thingy if necessary 
  (https://github.com/DoctorWkt/acwj/tree/master/34_Enums_and_Typedefs)
* think about array size initialization and constant folding and whether we need this or not.
  currently we cannot do 
  let int SOME_CONSTANT = 128;
  let int arr[SOME_CONSTANT + 4];
  (https://github.com/DoctorWkt/acwj/tree/master/44_Fold_Optimisation)
  (https://github.com/DoctorWkt/acwj/blob/master/45_Globals_Again/Readme.md)
  we can also go the route to just always heap alloc our arrays and make them stretchy
* 

DONE:

x get rid of all the parentheses in the output. we could use the fact that our precedences match
  that of C itself. therefore we could make parenthesized expression explicit in the AST/emitter
  and only use parentheses for ASTKind::ParenthesizesExpression. We would need to be 
  careful though with the type and lvalueness of such an expression. So we need to make sure
  that postfix operators like [] and -> still work for parenthesized expressions
  
*/

fun void Compile(String inputFilepath, String outputFilepath) {
    let Source source = PreprocessFile(inputFilepath);

    let SymbolTable* symbolTable = SymbolTableCreate(nullptr);
    // TODO: We can define builtin things here. the question is if we need to. 
    //       we just could declare stuff in a header now that we can parse those

    let Parser parser = ParserCreate(source, symbolTable);
    let ASTNode* syntaxTree = ParseGlobalStatements(&parser);
    if (parser.tokenCur.kind != TokenKind::EndOfFile)
    {
        ReportError(
            TokenGetLocation(parser.tokenCur),
            "Expected EOF token after parsing file, instead got '%s'", 
            TokenKindToString(parser.tokenCur.kind).cstr
        );
    }

    let Binder binder = BinderCreate(source, syntaxTree);
    let ASTNode* boundTree = BinderBindTree(&binder);

    let Emitter emitter = EmitterCreate(outputFilepath);
    EmitRoot(&emitter, boundTree);
}

fun void main(int argc, char** argv)
{
    if (argc != 3) {
      fprintf(stderr, "Expects two arguments: <inputfilepath> <outputfilepath>\n");
      exit(1);
    }

    Compile(StringCreateFromCStr(argv[1]), StringCreateFromCStr(argv[2]));
}