--  This test procedure for PDF_Out is in the Ada 95 syntax,
--  for compatibility with a larger number of development systems.
--  With Ada 2005 and later, you can also write "pdf.Write(...)" etc. everywhere.
--

with PDF_Out;                         use PDF_Out;

with Ada.Streams.Stream_IO;
with Ada.Text_IO;

procedure PDF_Out_Demo is

  procedure Small_demo is
    pdf: PDF_Out_File;
  begin
    Create(pdf, "Small.pdf");
    Page_Setup(pdf, A4_portrait);
    Put_Line(pdf, "This is a small demo for PDF_Out.");
    Put(pdf, "If you like numbers, here are some: ");
    for i in 1..10 loop
      Put(pdf, i);
      Put(pdf, "... ");
    end loop;
    New_Line(pdf);
    Put(pdf, "Fun, isn't it ?");
    Page_Setup(pdf, A4_landscape);
    New_Page(pdf);
    Put_Line(pdf, "Just had a page break (and switched to landscape)...");
    Close(pdf);
  end Small_demo;

  procedure Large_demo is
    page_nb: Natural:= 0;

    function Large_demo_contents return String is
      pdf: PDF_Out_String;
    begin
      Create(pdf);
      Page_Setup(pdf, A4_portrait);
      Put_Line(pdf, "This is a big demo for PDF_Out.");
      New_Line(pdf);
      Put_Line(pdf, "Page" & Integer'Image(Page_Count(pdf)) & " /" & Integer'Image(page_nb));
      --  !! ^ Will do a proper footer...
      Page_Setup(pdf, A4_landscape);
      New_Page(pdf);
      Put_Line(pdf, "Just had a page break (and switched to landscape)...");
      Put_Line(pdf, "Page" & Integer'Image(Page_Count(pdf)) & " /" & Integer'Image(page_nb));
      --  !! ^ Will do a proper footer...
      page_nb:= Page_Count(pdf);
      Close(pdf);
      return Contents(pdf);
    end Large_demo_contents;

    use Ada.Streams.Stream_IO;
    f: File_Type;
    --  Here page_nb is properly set (TeX-style processing):
    first_pass: constant String:= Large_demo_contents;
    pragma Unreferenced (first_pass);
  begin
    Create(f, Out_File, "Large.pdf");
    String'Write(Stream(f), Large_demo_contents);
    Close(f);
  end Large_demo;

  use Ada.Text_IO;

begin
  Put_Line("Small demo -> Small.pdf");
  Small_demo;
  Put_Line("Large demo -> Large.pdf");
  Large_demo;
end PDF_Out_Demo;
