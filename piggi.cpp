#include "parser.cpp"
#include "emitter.cpp"
#include "binder.cpp"

#if 0
import "parser.cpp"
import "emitter.cpp"
import "binder.cpp"
#endif

fun String ReadFileToString(String filepath) {
    let char* buffer = nullptr;
    let FILE* handle = fopen (filepath.cstr, "rb");
    if (handle == nullptr) {
        fprintf(stderr, "Unable to open file '%s'\n", filepath.cstr);
        exit(1);
    }

    fseek(handle, 0, SEEK_END);
    let longint length = (as longint)ftell(handle);
    fseek(handle, 0, SEEK_SET);
    buffer = (as char*)malloc(length + 1);
    if (buffer)
        fread(buffer, 1, length, handle);

    buffer[length] = '\0';
    fclose(handle);

    let String result;
    result.cstr = buffer;
    result.length = (as int)length;
    return result;
}

fun Source LoadSource(String modulename, String filepath) {
    let String content = ReadFileToString(filepath);

    let Source result;
    result.modulename = modulename;
    result.filepath = filepath;
    result.content = content;
    return result;
}

fun Source LoadModule(String modulename) {
    // TODO: Actually do some searching first if the filepath cannot be found
    // For example we may want to search "math.pig" also in subfolders called "math"
    let String modulepath = modulename;
    let String extensionSource = StringCreateFromCStr(".cpp");
    let String extensionHeader = StringCreateFromCStr(".hpp");
    if (!StringEndsWith(modulepath, extensionHeader) && !StringEndsWith(modulepath, extensionSource)) 
        modulepath = StringAppend(modulepath, extensionHeader);

    return LoadSource(modulename, modulepath);
}

fun void CollectSyntaxTreesRecursive(SyntaxTreeArray* result, StringArray* visited, String modulename)
{
    if (StringArrayContains(visited, modulename))
        return;
    else 
        StringArrayPush(visited, modulename);

    let Source source = LoadModule(modulename);
    let Parser parser = ParserCreate(source);
    let SyntaxTree* syntaxTree = ParseModule(&parser);

    for (let int index = 0; index < syntaxTree->moduleRoot->globalStatements.count; index += 1) {
        let SyntaxNode* statement = syntaxTree->moduleRoot->globalStatements.nodes[index]; 
        if (statement->kind == SyntaxKind::ImportDeclarationStatement) {
            let String importmodulename = statement->importStmt.modulenameLiteral.stringValueWithoutQuotes;
            CollectSyntaxTreesRecursive(result, visited, importmodulename);
        }
    }

    SyntaxTreeArrayPush(result, syntaxTree);
}

fun SyntaxTreeArray CollectSyntaxTrees(String rootModule)
{
    let SyntaxTreeArray result = SyntaxTreeArrayCreate();
    let StringArray visited = StringArrayCreate();
    CollectSyntaxTreesRecursive(&result, &visited, rootModule);
    return result;
}


fun void Compile(String inputFilepath, String outputFilepath) {
    let SyntaxTreeArray trees = CollectSyntaxTrees(inputFilepath);
    let SymbolTable* symbolTable = SymbolTableCreate(nullptr);
    // TODO: We can define builtin things in the symboltable here. the question is if we need to. 
    //       we just could declare stuff in a header now that we can parse those
    let Binder binder = BinderCreate(symbolTable);
    let ASTNode* boundTree = BindCompilationUnit(&binder, trees);

    let Emitter emitter = EmitterCreate(outputFilepath);
    EmitRoot(&emitter, boundTree);
}

fun void main(int argc, char** argv)
{
    if (argc == 1 || argc % 2 != 1) {
        fprintf(stderr, "Expects a multiple of two arguments: <inputfilepath> <outputfilepath>\n");
        exit(1);
    }

    let int counter = 1;
    while (counter < argc) {
        Compile(StringCreateFromCStr(argv[counter]), StringCreateFromCStr(argv[counter + 1]));
        counter += 2;
    }
}