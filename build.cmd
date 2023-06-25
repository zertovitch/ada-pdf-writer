@echo off

gprbuild -P pdf_out_gnat

if %errorlevel% == 9009 goto error

echo Press Return
pause
goto :eof


:error

echo.
echo The GNAT Ada compiler was not found in the PATH!
echo.
echo Check https://www.adacore.com/download for GNAT
echo or https://alire.ada.dev/ for ALIRE.
echo The PDF project is available as an ALIRE crate.
echo.
echo Press Return
pause
