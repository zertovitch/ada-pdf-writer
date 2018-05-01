--  Testing conformance of PDF output.
--  Provided by Giuseppe Cannone, 2018-04-29
--
--  Online validation test:
--  https://www.pdf-online.com/osa/validate.aspx
--
with PDF_Out;                           use PDF_Out;

procedure Validation_test is
  pdf: PDF_Out_File;
  box: Rectangle;
begin
  pdf.Create ("validation_test.pdf");
  pdf.Page_Setup (A4_portrait);
  --
  --  Simple vector graphics
  --
  box:= (10.0*one_cm, 24.7*one_cm, 5.0*one_cm, 5.0*one_cm);
  pdf.Draw (box, stroke);

  pdf.Close;
end Validation_test;
