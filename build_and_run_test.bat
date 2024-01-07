@echo off

REM ================================================================================================
REM Some setup busywork

if not defined DevEnvDir (
    call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat"
)

if exist bin rmdir /s /q bin
mkdir bin

echo ================================================================================================
echo PHASE 0 - Compile piggi with CPP compiler
echo:


pushd bin
cl.exe /W2 /WX /Zi /EHsc /nologo ../piggi.cpp /Fepiggi0 /Fopiggi0
if %errorlevel% neq 0 (
    popd
    goto :error
)
popd

echo PHASE 0 - SUCCESS
echo:

echo ================================================================================================
echo PHASE 1 - Transpile test program and piggi with piggi
echo:

bin\piggi0.exe test.cpp bin/test1.c
if %errorlevel% neq 0 (
    goto :error
)
bin\piggi0.exe piggi.cpp bin/piggi1.c
if %errorlevel% neq 0 (
    goto :error
)

pushd bin
cl.exe /W2 /WX /Zi /EHsc /nologo test1.c
if %errorlevel% neq 0 (
    popd
    goto :error
)
cl.exe /W2 /WX /Zi /EHsc /nologo piggi1.c
if %errorlevel% neq 0 (
    popd
    goto :error
)
popd


bin\test1.exe
if %errorlevel% neq 0 (
    goto :error
)

echo PHASE 1 - SUCCESS
echo:

echo ================================================================================================
echo PHASE 2 - Transpile piggi again, this time with the transpiled piggi
echo:

bin\piggi1.exe test.cpp bin/test2.c
if %errorlevel% neq 0 (
    goto :error
)
bin\piggi1.exe piggi.cpp bin/piggi2.c
if %errorlevel% neq 0 (
    goto :error
)

fc.exe bin\test1.c bin\test2.c
if %errorlevel% neq 0 (
    echo transpiled files differ
    goto :error
)
fc.exe bin\piggi1.c bin\piggi2.c
if %errorlevel% neq 0 (
    echo transpiled files differ
    goto :error
)

pushd bin
cl.exe /WX /W2 /Zi /EHsc /nologo test2.c
if %errorlevel% neq 0 (
    popd
    goto :error
)
cl.exe /WX /W2 /Zi /EHsc /nologo piggi2.c
if %errorlevel% neq 0 (
    popd
    goto :error
)
popd


bin\test2.exe
if %errorlevel% neq 0 (
    goto :error
)

echo PHASE 2 - SUCCESS
echo:

echo ================================================================================================
echo PHASE 3 - Repeat the previous step one more time
echo:

bin\piggi2.exe test.cpp bin/test3.c
if %errorlevel% neq 0 (
    goto :error
)
bin\piggi2.exe piggi.cpp bin/piggi3.c
if %errorlevel% neq 0 (
    goto :error
)

fc.exe bin\test2.c bin\test3.c
if %errorlevel% neq 0 (
    echo transpiled files differ
    goto :error
)
fc.exe bin\piggi2.c bin\piggi3.c
if %errorlevel% neq 0 (
    echo transpiled files differ
    goto :error
)

pushd bin
cl.exe /WX /W2 /Zi /EHsc /nologo test3.c
if %errorlevel% neq 0 (
    popd
    goto :error
)
cl.exe /WX /W2 /Zi /EHsc /nologo piggi3.c
if %errorlevel% neq 0 (
    popd
    goto :error
)
popd


bin\test3.exe
if %errorlevel% neq 0 (
    goto :error
)

echo PHASE 3 - SUCCESS
echo:

REM ================================================================================================

goto :success

REM ------------------------------------------------------------------------------------------------
:error

echo Failed with error #%errorlevel%.
exit /b %errorlevel%

REM ------------------------------------------------------------------------------------------------
:success
