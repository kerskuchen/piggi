#include "definitions.hpp"
// #if 0
// import "definitions.hpp"
// #endif

fun SourceFile ReadFileToString(String filepath) {
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

    let SourceFile result;
    result.filepath = filepath;
    result.content.cstr = buffer;
    result.content.length = (as int)length;
    return result;
}

struct Preprocessor {
    Source source;
    SourceFileArray seenFiles;
};

fun Preprocessor PreprocessorCreate() {
    let Preprocessor result;
    result.source = SourceCreateEmpty();
    result.seenFiles = SourceFileArrayCreate();
    return result;
}

fun bool PreprocessorSeenFile(Preprocessor* preprocessor, String filepath) {
    for (let int index = 0; index < preprocessor->seenFiles.count; index += 1) {
        if (StringEquals(preprocessor->seenFiles.files[index].filepath, filepath)) {
            return true;
        }
    }
    return false;
}

fun void PreprocessorPreprocessFile(Preprocessor* preprocessor, String filepath) {
    if (PreprocessorSeenFile(preprocessor, filepath))
        return;

    let SourceFile sourceFile = ReadFileToString(filepath);
    SourceFileArrayPush(&preprocessor->seenFiles, sourceFile);

    // TODO: get rid of all this nonesense
    let Source dummy = SourceCreateEmpty();
    SourceFileArrayPush(&dummy.files, sourceFile);
    dummy.content = sourceFile.content;
    let SyntaxTree tree;
    tree.moduleRoot = nullptr;
    tree.source = dummy;
    let Scanner scanner = ScannerCreate(&tree);

    let String result = StringCreateEmpty();
    while (true) {
        let SyntaxToken token = NextToken(&scanner);
        if (token.kind == SyntaxKind::EndOfFileToken)
            break;
        if (token.kind != SyntaxKind::IncludeDirectiveKeyword)
            continue;


        token = NextToken(&scanner);
        if (token.kind == SyntaxKind::LessToken) {
            token = NextToken(&scanner); // ex. stdio
            token = NextToken(&scanner); // .
            assert(token.kind == SyntaxKind::DotToken);
            token = NextToken(&scanner); // ex. hpp
            token = NextToken(&scanner); // >
            assert(token.kind == SyntaxKind::GreaterToken);
        } else if (token.kind == SyntaxKind::StringLiteralToken) {
            PreprocessorPreprocessFile(preprocessor, token.stringValueWithoutQuotes);
        } else {
            ReportError(
                TokenGetLocation(token),
                "Expected filepath after '#include' directive in '%s'",
                filepath.cstr
            );
        }
    }

    SourceFileArrayPush(&preprocessor->source.files, sourceFile);
    preprocessor->source.content = StringAppend(preprocessor->source.content, sourceFile.content);
}

fun Source PreprocessFile(String filepath) {
    let Preprocessor preprocessor = PreprocessorCreate();
    PreprocessorPreprocessFile(&preprocessor, filepath);
    return preprocessor.source;
}