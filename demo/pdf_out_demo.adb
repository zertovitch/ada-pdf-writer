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
    for i in 1..10 loop
      Put(pdf, Real(i) * 0.1234, 3,4,0);
    end loop;
    New_Line(pdf);
    Put_Line(pdf, '[' & Img(3.14159) & ']');
    Put_Line(pdf, '[' & Img(3.141592653) & ']');
    Put(pdf, "Fun, isn't it ?");
    Page_Setup(pdf, A4_landscape);
    New_Page(pdf);
    Put_Line(pdf, "Just had a page break (and switched to landscape)...");
    Close(pdf);
  end Small_demo;

  procedure Big_demo is

    mem_page_nb: Natural:= 0;
    curve_max: constant := 8;

    function Big_demo_contents return String is
      pdf: Fancy_PDF;
      factor: Real;
    begin
      pdf.page_nb:= mem_page_nb;
      Create(pdf);
      Title(pdf, "Big demo for Ada PDF Writer");
      Author(pdf, "Zerte");
      Keywords(pdf, "Ada, PDF");
      Title(pdf, "Big demo for Ada PDF Writer");
      Creator_Application(pdf, "PDF_Out_Demo");
      Page_Setup(pdf, A4_portrait);
      Top_Margin(pdf, one_cm * 4.0);
      --
      Put_Line(pdf, "This is a big demo for Ada PDF Writer (PDF_Out package).");
      Put_Line(pdf, "We begin with some Put_Line and other Ada.Text_IO-like commands.");
      Put_Line(pdf, "You can use a PDF stream just like a File_Type of Ada.Text_IO");
      Put_Line(pdf, "and its standard subprograms.");
      New_Line(pdf);
      Put_Line(pdf, "Of course you have much more with PDF_Out: high-quality fonts,");
      Put_Line(pdf, "colors, amazing vector graphics, image inclusions, ...");
      New_Line(pdf, 2);
      Line_Spacing(pdf, 2.0);
      for r in 1..5 loop
        Color(pdf, (Real(r) * 0.2, 0.0, 0.0));
        Put_Line(pdf, "Variations of red...");
      end loop;
      Line_Spacing(pdf, default_line_spacing);
      for n in 1..4 loop
        factor:= Real(n) * 0.03;
        Image(pdf,
          "demo/bordu_2016_01_01_25pct.jpg",
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
      declare
        z_from: constant String:= "(EXM) EXEMPLAR AIRPORT";
        z_to  : constant String:= "(DEM) DEMO INTL AIRPORT";
        z_date: constant String:= "08 JAN 2016";
        --
        procedure Boarding_pass(
          y : Real;
          passenger, from, to, date : String
        )
        is
          factor_bp: constant:= 0.6;
          x0: constant Real:= Left_Margin(pdf) * 2.0;
          y0: constant Real:= Bottom_Margin(pdf) + y;
        begin
          Image(pdf, "demo/bp_mask.jpg", (x0, y0, factor_bp * 835.0, factor_bp * 315.0));
          Put_XY(pdf, x0 +  15.0, y0 + 120.0, date);
          Put_XY(pdf, x0 + 158.0, y0 + 120.0, from);
          Put_XY(pdf, x0 + 158.0, y0 +  87.0, to);
          Put_XY(pdf, x0 + 158.0, y0 +  15.0, passenger);
        end Boarding_pass;
      begin
        Boarding_pass(200.0, "ZERTE, JULES", z_from, z_to, z_date);
        Boarding_pass(  0.0, "ZERTE, ROMEOTTE", z_from, z_to, z_date);
      end;
      --
      --  Some vector graphics
      --
      New_Page(pdf);
      --  Testing line widths
      for i in 1..8 loop
        Line_Width(pdf, Real(i));
        Single_Line(
          pdf,
          (Left_Margin(pdf),         150.0 + 20.0 * Real(i)),
          (Left_Margin(pdf) + 500.0, 350.0 + 20.0 * Real(i))
        );
      end loop;
      --  Blue star
      Line_Width(pdf, 5.0);
      Stroking_Color(pdf, (0.0,1.0,0.0));
      Color(pdf, (0.0,0.0,1.0));
      --  Fill only: equivalent to
      --    Insert_PDF_Code(pdf, "315 226 m 299 182 l 339 208 l 291 208 l 331 182 l f");
      --
      for r in reverse fill .. stroke loop
        Move(pdf, (315.0, 226.0));
        Line(pdf, (299.0, 182.0));
        Line(pdf, (339.0, 208.0));
        Line(pdf, (291.0, 208.0));
        Line(pdf, (331.0, 182.0));
        Finish_Path(pdf, True, r, nonzero_winding_number);
      end loop;
      Page_Setup(pdf, A4_portrait);
      New_Page(pdf);
      Stroking_Color(pdf, black);
      Line_Width(pdf, initial_line_width);
      declare
        procedure Bezier_curves_demo(o: Point) is
          f: constant:= 0.3;
        begin
          Move(pdf, o + f * (350.0, 350.0));
          Line(pdf, o + f * (350.0, 400.0));
          Cubic_Bezier(pdf, o + f * (350.0, 475.0), o + f * (250.0, 475.0), o + f * (250.0, 400.0));
          Cubic_Bezier(pdf, o + f * (250.0, 350.0), o + f * (325.0, 350.0), o + f * (325.0, 400.0));
          Cubic_Bezier(pdf, o + f * (325.0, 437.5), o + f * (275.0, 437.5), o + f * (275.0, 400.0));
          Cubic_Bezier(pdf, o + f * (275.0, 382.0), o + f * (300.0, 382.0), o + f * (300.0, 400.0));
        end;
        y0: Real:= 600.0;
      begin
        for rend in Path_Rendering_Mode loop
          for rule in Inside_path_rule loop
            for close_it in reverse Boolean loop
              Color(pdf, (0.1,1.0,1.0));
              Bezier_curves_demo((0.0, y0-50.0));
              Finish_Path(pdf, close_it, rend, rule);
              Color(pdf, black);
              Put_XY(pdf, 150.0, y0+70.0,
                "rend = " & Path_Rendering_Mode'Image(rend) &
                ", rule = " & Inside_path_rule'Image(rule) &
                ", close path = " & Boolean'Image(close_it)
              );
              y0 := y0 - 45.0;
            end loop;
          end loop;
        end loop;
      end;
      --
      --  Finishing
      --
      mem_page_nb:= Page(pdf);
      Close(pdf);
      return Contents(pdf);
    end Big_demo_contents;

    use Ada.Streams.Stream_IO;
    f: File_Type;
    --  Here page_nb is properly set (TeX-style processing):
    first_pass: constant String:= Big_demo_contents;
    pragma Unreferenced (first_pass);
  begin
    Create(f, Out_File, "Big.pdf");
    String'Write(Stream(f), Big_demo_contents);
    Close(f);
  end Big_demo;

  use Ada.Text_IO;

begin
  Put_Line("Small demo -> Small.pdf");
  Small_demo;
  Put_Line("Big demo -> Big.pdf");
  Big_demo;
end PDF_Out_Demo;
