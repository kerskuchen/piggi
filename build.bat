@echo off

if not defined DevEnvDir (
    call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat"
)

if not exist bin mkdir bin
pushd bin
cl.exe /W2 /WX /Zi /EHsc /nologo ../piggi.cpp
if %errorlevel% neq 0 (
    popd
    goto :error
)
popd