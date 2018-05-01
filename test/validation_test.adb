--  Testing conformance of PDF output.
--  Provided by Giuseppe Cannone, 2018-04-29
--
--  A third-party online validation test can be found here:
--  https://www.pdf-online.com/osa/validate.aspx
--
with PDF_Out;

procedure Validation_test is
  use PDF_Out;
  pdf: PDF_Out_File;
  o: constant Point := (0.0, 550.0);
  f: constant:= 0.3;
begin
  --  We split the test into very small ones since the
  --  validator doesn't give locations of eventual errors.
  --
  for test in 1 .. 2 loop
    pdf.Create ("validation test" & test'Image & ".pdf");
    pdf.Page_Setup (A4_portrait);
    case test is
      when 1 =>  --  Simple vector graphics
        pdf.Draw ((10.0*one_cm, 24.7*one_cm, 5.0*one_cm, 5.0*one_cm), stroke);
      when 2 =>
        pdf.Line_Width(5.0 * initial_line_width);
        --  Move starts a [sub]path
        pdf.Move(o + f * (350.0, 350.0));
        --  The online validator doesn't like text-mode stuff within a path
        pdf.Line(o + f * (350.0, 400.0));
        pdf.Finish_Path(True, stroke, nonzero_winding_number);
    end case;
    pdf.Close;
  end loop;
end Validation_test;
