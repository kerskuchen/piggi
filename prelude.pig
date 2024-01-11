// <stdlib.h>
extern fun void* malloc(longint size);
extern fun void* realloc(void* oldLocation, longint size);
extern fun void exit(longint value);
extern let longint UCHAR_MIN;
extern let longint UCHAR_MAX;
extern let longint CHAR_MIN;
extern let longint CHAR_MAX;
extern let longint SHRT_MIN;
extern let longint SHRT_MAX;
extern let longint INT_MIN;
extern let longint INT_MAX;

// <assert.h>
extern fun void assert(longint value);

// <stdarg.h>
extern struct va_list { int dummy; };
extern fun void va_start(va_list args, cstring fmt);
extern fun void va_end(va_list args);

// <stdio.h>
extern struct FILE;
extern let int SEEK_END;
extern let int SEEK_SET;
extern let FILE* stderr;
extern let FILE* stdout;
extern let FILE* stdin;
extern fun FILE* fopen(cstring filepath, cstring options);
extern fun int fclose(FILE* file);
extern fun void fseek(FILE* file, longint offset, int origin);
extern fun longint ftell(FILE* file);
extern fun longint fread(void* outBuffer, longint elemSize, longint elementCount, FILE* file);
extern fun void vfprintf(FILE* outStream, cstring fmt, va_list args);
extern fun void fprintf(FILE* outStream, cstring fmt, ...);
extern fun int snprintf(char* buffer, int bufferSize, cstring fmt, ...);