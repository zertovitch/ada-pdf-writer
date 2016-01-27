echo with PDF_Out; use PDF_Out;>small_demo.adb
echo.>>small_demo.adb
echo procedure Small_demo is>>small_demo.adb
echo   pdf: PDF_Out_File;>>small_demo.adb
echo begin>>small_demo.adb
echo   pdf.Create("Small.pdf");>>small_demo.adb
echo   pdf.Put_Line("This is a very small demo for PDF_Out...");>>small_demo.adb
echo   pdf.Close;>>small_demo.adb
echo end Small_demo;>>small_demo.adb

rem Call GNATMake without project file: we want the .ali here.

gnatmake ..\demo\pdf_out_demo.adb -I.. -I..\gid -I..\demo -j3
gnatmake small_demo.adb -I.. -I..\gid -j3

rem Small_Demo without local references
perl pw_html.pl small_demo -d -I.. -I..\gid -opw_html
perl pw_html.pl pdf_out_demo pdf_out.ads pdf_out.adb -I.. -I..\gid -I..\demo -f -d -opw_html

del *.ali
del *.o
