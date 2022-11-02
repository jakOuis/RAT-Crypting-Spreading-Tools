@echo off

cd %~dp0

goto sign
:sign

@echo Sign File King.dll
set message=%DATE%
date 01.03.2014
signtool.exe sign /v /ac 1111222.cer /f current_cert.pfx /p nv1d1aRules %1
date %message%
:end
