echo with PDF_Out;>hello.adb
echo.>>hello.adb
echo procedure Hello is>>hello.adb
echo   pdf : PDF_Out.PDF_Out_File;>>hello.adb
echo begin>>hello.adb
echo   pdf.Create ("tiny.pdf");>>hello.adb
echo   pdf.Put_Line ("Hello world !);>>hello.adb
echo   pdf.Close;>>hello.adb
echo end Hello;>>hello.adb

rem Call GNATMake without project file: we want the .ali here.

gnatmake ..\demos\pdf_out_demo.adb    -I.. -I..\gid -I..\demos -I..\tests -I..\tools -j0
gnatmake ..\tests\page_test.adb       -I.. -I..\gid -I..\demos -I..\tests -I..\tools -j0
gnatmake ..\tests\validation_test.adb -I.. -I..\gid -I..\demos -I..\tests -I..\tools -j0
gnatmake ..\tools\img2pdf.adb         -I.. -I..\gid -I..\demos -I..\tests -I..\tools -j0

gnatmake hello.adb -I.. -I..\gid -j0

rem Small_Demo without local references
perl pw_html.pl     small_demo -d -I.. -I..\gid -opw_html
rem The rest with local references
perl pw_html.pl     pdf_out_demo pdf_out.ads pdf_out.adb img2pdf.adb page_test.adb validation_test.adb -I.. -I..\gid -I..\demo -I..\test -I..\tools -f -d -opw_html

del *.ali
del *.o
del *.exe
