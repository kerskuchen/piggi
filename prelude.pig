// <stdlib.h>
extern fun malloc(size: long): void*
extern fun realloc(oldLocation: void*, size: long): void* 
extern fun exit(value: long)
extern let UCHAR_MIN: long
extern let UCHAR_MAX: long
extern let CHAR_MIN: long
extern let CHAR_MAX: long
extern let SHRT_MIN: long
extern let SHRT_MAX: long
extern let INT_MIN: long
extern let INT_MAX: long

// <assert.h>
extern fun assert(value: long)

// <stdarg.h>
extern struct va_list
extern fun va_start(args: va_list, fmt: cstring)
extern fun va_end(args: va_list)

// <stdio.h>
extern struct FILE
extern let SEEK_END: int
extern let SEEK_SET: int
extern let stderr: FILE*
extern let stdout: FILE*
extern let stdin: FILE*
extern fun fopen(filepath: cstring, options: cstring): FILE*
extern fun fclose(file: FILE*): int
extern fun fseek(file: FILE*, offset: long, origin: int)
extern fun ftell(file: FILE*): long
extern fun fread(outBuffer: void*, elemSize: long, elementCount: long , file: FILE*): long
extern fun vfprintf(outStream: FILE*, fmt: cstring, args: va_list)
extern fun fprintf(outStream: FILE*, fmt: cstring, ..)
extern fun snprintf(buffer: char*, bufferSize: int, fmt: cstring, ..): int