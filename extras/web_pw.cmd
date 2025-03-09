echo with PDF_Out;>hello.adb
echo.>>hello.adb
echo procedure Hello is>>hello.adb
echo   pdf : PDF_Out.PDF_Out_File;>>hello.adb
echo begin>>hello.adb
echo   pdf.Create ("tiny.pdf");>>hello.adb
echo   pdf.Put_Line ("Hello world !);>>hello.adb
echo   pdf.Close;>>hello.adb
echo end Hello;>>hello.adb

gprbuild -P ..\pdf_out.gpr

rem Call GNATMake without project file (because of the extra example): we will have the .ali here.
gnatmake hello.adb -I.. -I../gid -aO../obj/debug -j0

set params=-opw_html -b#fffcfb -ipw_head.txt -jpw_top.txt -kpw_bottom.txt
set params=%params%  -I../obj/debug -I.. -I../gid -I../demos -I../tests -I../tools

rem Here we invoke GNATHTML ( https://github.com/zertovitch/ali_parse )

rem Hello (TBD for GNATHTML "-f" switch for local references: we don't want them for Hello)
gnathtml hello.adb %params%

rem The rest with local references
gnathtml pdf_out_demo.adb pdf_out.ads pdf_out.adb img2pdf.adb page_test.adb validation_test.adb color_pinstripe_printer.adb hilbert_curve.adb k_means.adb koch_curve.adb peano_curve.adb -f %params%

del hello.ali
del hello.o
del hello.exe
