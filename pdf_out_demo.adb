-- This test procedure for PDF_Out is in the Ada 95 syntax,
-- for compatibility with a larger number of development systems.
-- With Ada 2005 and later, you can also write "pdf.Write(...)" etc. everywhere.
--

with PDF_Out;                         use PDF_Out;

with Ada.Text_IO;

procedure PDF_Out_Demo is

  procedure Small_demo is
    xl: PDF_Out_File;
  begin
    Create(xl, "Small.pdf");
    Put_Line(xl, "This is a small demo for PDF_Out");
    Close(xl);
  end Small_demo;

  use Ada.Text_IO;

begin
  Put_Line("Small demo -> Small.pdf");
  Small_demo;
end PDF_Out_Demo;
