--  This test procedure for PDF_Out is in the Ada 95 syntax,
--  for compatibility with a larger number of development systems.
--  With Ada 2005 and later, you can also write "pdf.Write(...)" etc. everywhere.
--

with PDF_Out;                         use PDF_Out;

with Ada.Text_IO;

procedure PDF_Out_Demo is

  procedure Small_demo is
    pdf: PDF_Out_File;
  begin
    Create(pdf, "Small.pdf");
    Put_Line(pdf, "This is a small demo for PDF_Out.");
    Put(pdf, "If you like numbers, here are some: ");
    for i in 1..10 loop
      Put(pdf, i);
      Put(pdf, "... ");
    end loop;
    New_Line(pdf);
    Put(pdf, "Fun, isn't it ?");
    New_Page(pdf);
    Put_Line(pdf, "Just had a page break...");
    Close(pdf);
  end Small_demo;

  use Ada.Text_IO;

begin
  Put_Line("Small demo -> Small.pdf");
  Small_demo;
end PDF_Out_Demo;
