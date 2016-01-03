--  This test procedure for PDF_Out is in the Ada 95 syntax,
--  for compatibility with a larger number of development systems.
--  With Ada 2005 and later, you can also write "pdf.Write(...)" etc. everywhere.
--

with PDF_Out;                         use PDF_Out;
with Fancy_page;                      use Fancy_page;

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

    mem_page_nb: Natural:= 0;
    curve_max: constant := 8;

    function Large_demo_contents return String is
      pdf: Fancy_PDF;
      factor: Real;
    begin
      pdf.page_nb:= mem_page_nb;
      Create(pdf);
      Page_Setup(pdf, A4_portrait);
      Put_Line(pdf, "This is a big demo for PDF_Out.");
      Put_Line(pdf, "We begin with some Put_Line and other Ada.Text_IO-like commands.");
      Put_Line(pdf, "You can use a PDF stream just like a File_Type of Ada.Text_IO");
      Put_Line(pdf, "and its standard subprograms.");
      New_Line(pdf);
      Put_Line(pdf, "Of course you have much more with PDF_Out: high-quality fonts,");
      Put_Line(pdf, "colors, amazing vector graphics, image inclusions, ...");
      New_Line(pdf, 2);
      for r in 1..5 loop
        Color(pdf, (Real(r) * 0.2, 0.0, 0.0));
        Put_Line(pdf, "Variations of red...");
      end loop;
      for n in 1..4 loop
        factor:= Real(n) * 0.03;
        Image(pdf,
          "bordu_2016_01_01_25pct.jpg",
          (Left_Margin(pdf)   + Real(n)* 60.0,
           Bottom_Margin(pdf) + Real(n)* 100.0 - 50.0,
           factor * 1296.0,
           factor * 864.0)
        );
      end loop;
      Color(pdf, black);
      for x in -curve_max..curve_max loop
        Text_XY(pdf,
          Left_Margin(pdf) + Real(curve_max + x) * one_cm,
          Bottom_Margin(pdf) + ((Real(x)*0.4) ** 2) * one_cm
        );
        Put(pdf, Integer'Image(x));
      end loop;
      New_Line(pdf);
      Page_Setup(pdf, A4_landscape);
      New_Page(pdf);
      Put_Line(pdf, "Just had a page break (and switched to landscape)...");
      mem_page_nb:= Page(pdf);
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
