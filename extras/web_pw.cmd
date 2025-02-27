echo with PDF_Out;>hello.adb
echo.>>hello.adb
echo procedure Hello is>>hello.adb
echo   pdf : PDF_Out.PDF_Out_File;>>hello.adb
echo begin>>hello.adb
echo   pdf.Create ("tiny.pdf");>>hello.adb
echo   pdf.Put_Line ("Hello world !);>>hello.adb
echo   pdf.Close;>>hello.adb
echo end Hello;>>hello.adb

gprbuild -P ..\pdf_out_gnat.gpr

rem Call GNATMake without project file (because of the extra example): we will have the .ali here.
gnatmake hello.adb -I.. -I..\gid -aO..\obj_debug -j0

rem Hello without local references
perl pw_html.pl     hello.adb -d -I.. -I..\gid -opw_html

rem The rest with local references
perl pw_html.pl     pdf_out_demo.adb pdf_out.ads pdf_out.adb img2pdf.adb page_test.adb validation_test.adb color_pinstripe_printer.adb hilbert_curve.adb k_means.adb koch_curve.adb peano_curve.adb -I..\obj_debug -I.. -I..\gid -I..\demos -I..\tests -I..\tools -f -d -opw_html

del *.ali
del *.o
del *.exe
