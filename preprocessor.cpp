#include "definitions.hpp"

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
};

fun Preprocessor PreprocessorCreate() {
    let Preprocessor result;
    result.source = SourceCreateEmpty();
    return result;
}

fun void PreprocessorPreprocessFile(Preprocessor* preprocessor, String filepath) {
    if (SourceContainsFile(preprocessor->source, filepath))
        return;

    let SourceFile sourceFile = ReadFileToString(filepath);
    let Source dummy = SourceCreateEmpty();
    SourceFileArrayPush(&dummy.files, sourceFile);
    dummy.content = sourceFile.content;
    let Scanner scanner = ScannerCreate(dummy);

    let String result = StringCreateEmpty();
    while (true) {
        let Token token = NextToken(&scanner);
        if (token.kind == TokenKind::EndOfFile)
            break;
        if (token.kind != TokenKind::IncludeDirective)
            continue;


        token = NextToken(&scanner);
        if (token.kind == TokenKind::Less) {
            token = NextToken(&scanner); // ex. stdio
            token = NextToken(&scanner); // .
            assert(token.kind == TokenKind::Dot);
            token = NextToken(&scanner); // ex. hpp
            token = NextToken(&scanner); // >
            assert(token.kind == TokenKind::Greater);
        } else if (token.kind == TokenKind::StringLiteral) {
            PreprocessorPreprocessFile(preprocessor, token.stringValue);
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