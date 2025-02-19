with PDF_Out;

with Bar_code,
     Fancy_Page,
     Insert_Mascot,
     QR_code;

with Ada.Numerics,
     Ada.Text_IO;

procedure PDF_Out_Demo is

  use PDF_Out;

  procedure Small_Demo is
    pdf : PDF_Out_File;
  begin
    pdf.Create ("Small.pdf");
    pdf.Page_Setup (A4_portrait);
    pdf.Put_Line ("This is a small demo for PDF_Out.");
    pdf.Put ("If you like numbers, here are some: ");
    for i in 1 .. 10 loop
      pdf.Put (i);
      pdf.Put ("... ");
    end loop;
    pdf.New_Line;
    for i in 1 .. 10 loop
      pdf.Put (Real (i) * 0.1234, 3, 4, 0);
    end loop;
    pdf.New_Line;
    pdf.Put_Line ('[' & Img (3.14159) & ']');
    pdf.Put_Line ('[' & Img (3.141592653) & ']');
    pdf.Put ("Fun, isn't it ?");
    pdf.Page_Setup (A4_landscape);
    pdf.New_Page;
    pdf.Put_Line ("Just had a page break (and switched to landscape)...");
    pdf.Close;
  end Small_Demo;

  procedure Big_Demo is
    pdf : Fancy_Page.Fancy_PDF;

    procedure Arc_Demo is  --  Reused from arc_test.adb.
      subtype Pie_Value is Float range 0.0 .. Float'Last;

      type Pie_Slice is record
        value : Pie_Value;
        color : PDF_Out.Color_Type;
        --  Labels, etc. ...
      end record;

      type Pie_Contents is array (Positive range <>) of Pie_Slice;

      pie_1 : constant Pie_Contents :=
        ((123.0, (1.0, 0.5, 0.5)),
         (456.0, (0.5, 1.0, 0.5)),
         (789.0, (0.5, 0.5, 1.0)));

      pull_apart : constant := -1;  --  All slices are pulled out
      pull_none  : constant :=  0;  --  No slice is pulled out

      procedure Pie
        (center      : Point;
         radius      : Real;
         start_angle : Real;
         p           : Pie_Contents;
         pull_out    : Integer := pull_none)
      is
        total : Pie_Value := 0.0;
        angle_1, angle_2, angle_mid : Real := start_angle;
        shift : Point;
        use Ada.Numerics, Real_Elementary_Functions;
        deg_to_rad  : constant := Pi / 180.0;
        pull_factor : Real := 0.25;
      begin
        if pull_out = pull_apart then
          pull_factor := pull_factor * 0.5;
        end if;
        for i in p'Range loop
          total := total + p (i).value;
        end loop;
        for i in p'Range loop
          angle_2 := angle_2 + Real (p (i).value / total) * 360.0;
          if i = pull_out or pull_out = pull_apart then
            angle_mid := deg_to_rad * (angle_1 + angle_2) * 0.5;
            shift := radius * pull_factor * (Cos (angle_mid), Sin (angle_mid));
          else
            shift := (0.0, 0.0);
          end if;
          pdf.Move (shift + center);
          pdf.Filling_Color (p (i).color);
          pdf.Arc (shift + center, radius, angle_1, angle_2, True);
          angle_1 := angle_2;
          pdf.Line (shift + center);
          pdf.Finish_Path (False, fill_then_stroke, nonzero_winding_number);
        end loop;
      end Pie;

      radius : constant := 40.0;

    begin
      pdf.Stroking_Color ((0.25, 0.0, 0.0));
      pdf.Line_Width (2.0);
      for pull in pull_apart .. 3 loop
        Pie
          ((pdf.Layout.x_min + pdf.Margins.left + radius,
            pdf.Layout.y_min + pdf.Layout.height - pdf.Margins.top - radius * (1.0 + Real (1 + pull) * 2.5)),
           radius,
           90.0,
           pie_1,
           pull);
      end loop;
    end Arc_Demo;

    procedure Bar_Code_Demo is
      x : constant Real := pdf.Layout.x_min + pdf.Margins.left;
      y : constant Real := pdf.Layout.y_min + pdf.Layout.height - pdf.Margins.top;
      width  : constant Real := pdf.Layout.width - pdf.Margins.right - pdf.Margins.left;
      height : constant Real := pdf.Get_Font_Size + 5.0;
      --
      url_rect : constant Rectangle :=
        (x - 2.0,
         y - 4.0,
         width + 4.0,
         height);
    begin
      pdf.Put_XY
        (x,
         y,
         "Direct PDF vector graphics code inclusion - bar code and QR code from: http://ada-bar-codes.sf.net/");

      --  Hyperlink with a big green rectangle:
      pdf.Hyperlink (url_rect, False, "http://ada-bar-codes.sf.net/");
      pdf.Stroking_Color ((0.0, 0.6, 0.0));
      pdf.Draw (url_rect, stroke);

      pdf.Insert_Graphics_PDF_Code (Bar_code.code_in_pdf);
      pdf.Insert_Graphics_PDF_Code (QR_code.code_in_pdf);
    end Bar_Code_Demo;

    mem_page_nb : Natural := 0;
    curve_max : constant := 8;

    procedure Big_Demo_Contents (name : String) is
      factor : Real;
      bordu : constant String := "demos/bordu_2016_01_01_25pct.jpg";
      target : Rectangle;
    begin
      pdf.page_nb := mem_page_nb;
      pdf.Create (name);
      pdf.Title ("Big demo for Ada PDF Writer");
      pdf.Author ("Zerte");
      pdf.Keywords ("Ada, PDF");
      pdf.Creator_Application ("PDF_Out_Demo");
      pdf.Page_Setup (A4_portrait);
      pdf.Top_Margin (one_cm * 4.0);
      --
      pdf.Put_Line
        ("This is a big demo for Ada PDF Writer (PDF_Out package)."         & ASCII.LF &
         "We begin with some Put_Line and other Ada.Text_IO-like commands." & ASCII.LF &
         "You can use a PDF stream just like a File_Type of Ada.Text_IO"    & ASCII.LF &
         "and its standard subprograms.");
      pdf.New_Line;
      pdf.Put_Line ("Of course you have much more with PDF_Out: high-quality fonts,");
      pdf.Put_Line ("colors, amazing vector graphics, image inclusions, ...");
      pdf.New_Line;
      pdf.Put_Line ("Testing Latin-1 characters: je répète, ganz schöne Umläute");
      pdf.Put_Line ("Testing special characters: ()\");
      pdf.New_Line (2);
      pdf.Line_Spacing (2.0);
      for r in 1 .. 5 loop
        pdf.Color ((Real (r) * 0.19999, 0.0, 0.0));
        pdf.Put_Line ("Variations of red...");
      end loop;
      pdf.Line_Spacing (default_line_spacing);
      for n in 1 .. 4 loop
        factor := Real (n) * 0.03;
        target := Get_Pixel_Dimensions (bordu);
        pdf.Image
          (bordu,
           (pdf.Left_Margin, pdf.Bottom_Margin) +
           (Real (n) * 60.0, Real (n) * 100.0 - 50.0) +
            factor * target
          );
      end loop;
      pdf.Color (black);
      for x in -curve_max .. curve_max loop
        pdf.Text_XY
          (pdf.Left_Margin + Real (curve_max + x) * one_cm,
           pdf.Bottom_Margin + (1.0 + (Real (x) * 0.4) ** 2) * one_cm);
        pdf.Put (Integer'Image (x));
      end loop;
      pdf.New_Line;
      pdf.Finish_Page;  --  Needed for having the footer right before changing orientation.
      pdf.Page_Setup (A4_landscape);

      ----------------------------------
      --  Ada Airlines boarding pass  --
      ----------------------------------
      pdf.New_Page;
      pdf.Put_Line ("Just had a page break - and switched to landscape. Click on the pass -> go to page 7.");
      declare
        z_from : constant String := "(EXM) EXEMPLAR AIRPORT";
        z_to   : constant String := "(DEM) DEMO INTL AIRPORT";
        z_date : constant String := "08 JAN 2016";
        --
        procedure Boarding_pass (y : Real; passenger, from, to, date : String) is
          factor_bp : constant := 0.6;
          x0 : constant Real := pdf.Left_Margin * 2.0;
          y0 : constant Real := pdf.Bottom_Margin + y;
          bp_mask_area : constant Rectangle := (x0, y0, factor_bp * 835.0, factor_bp * 315.0);
        begin
          pdf.Image ("demos/bp_mask.jpg", bp_mask_area);
          pdf.Put_XY (x0 +  15.0, y0 + 120.0, date);
          pdf.Put_XY (x0 + 158.0, y0 + 120.0, from);
          pdf.Put_XY (x0 + 158.0, y0 +  87.0, to);
          pdf.Put_XY (x0 + 158.0, y0 +  15.0, passenger);
          pdf.Hyperlink (bp_mask_area, True, 7);
        end Boarding_pass;
      begin
        Boarding_pass (205.0, "ZERTE, JULES", z_from, z_to, z_date);
        Boarding_pass (5.0, "ZERTE, ROMEOTTE", z_from, z_to, z_date);
      end;
      ------------------------------
      --  Testing standard fonts  --
      ------------------------------
      pdf.New_Page;
      for std_fnt in Standard_Font_Type loop
        pdf.Font_Size (10.0);
        pdf.Font (Helvetica);
        pdf.Put ("Displaying standard font: " & Standard_Font_Type'Image (std_fnt) & ' ');
        pdf.Font (std_fnt);
        pdf.Font_Size (8.0);
        pdf.Put ("Hello! (8pt) ");
        pdf.Font_Size (16.0);
        pdf.Put ("Hello! (16pt) ");
        pdf.Font_Size (24.0);
        pdf.Put ("Hello! (24pt) ");
        pdf.New_Line;
      end loop;
      pdf.Font_Size (11.0);
      ------------------------------------------------------
      --  Some vector graphics - lines and closed shapes  --
      ------------------------------------------------------
      pdf.New_Page;
      --  Testing line widths
      for i in 1 .. 8 loop
        pdf.Line_Width (Real (i));
        pdf.Single_Line (
          (pdf.Left_Margin,         150.0 + 20.0 * Real (i)),
          (pdf.Left_Margin + 500.0, 350.0 + 20.0 * Real (i))
        );
      end loop;
      --  Blue star
      pdf.Line_Width (5.0);
      pdf.Stroking_Color ((0.0, 1.0, 0.0));
      pdf.Filling_Color  ((0.0, 0.0, 1.0));
      --
      --  Fill only: equivalent to
      --    pdf.Insert_PDF_Code ("315 226 m 299 182 l 339 208 l 291 208 l 331 182 l f");
      for r in reverse fill .. stroke loop
        pdf.Move ((315.0, 226.0));
        pdf.Line ((299.0, 182.0));
        pdf.Line ((339.0, 208.0));
        pdf.Line ((291.0, 208.0));
        pdf.Line ((331.0, 182.0));
        pdf.Finish_Path (True, r, nonzero_winding_number);
      end loop;
      pdf.Finish_Page;  --  Needed for having the footer right before changing orientation.
      pdf.Page_Setup (A4_portrait);
      ----------------------------------------------------------------------
      --  More advanced vector graphics - various ways of closing shapes  --
      ----------------------------------------------------------------------
      pdf.New_Page;
      declare
        procedure Bezier_Curves_Demo (o : Point) is
          f : constant := 0.3;
        begin
          pdf.Move (o + f * (350.0, 350.0));
          pdf.Line (o + f * (350.0, 400.0));
          pdf.Cubic_Bezier (o + f * (350.0, 475.0), o + f * (250.0, 475.0), o + f * (250.0, 400.0));
          pdf.Cubic_Bezier (o + f * (250.0, 350.0), o + f * (325.0, 350.0), o + f * (325.0, 400.0));
          pdf.Cubic_Bezier (o + f * (325.0, 437.5), o + f * (275.0, 437.5), o + f * (275.0, 400.0));
          pdf.Cubic_Bezier (o + f * (275.0, 382.0), o + f * (300.0, 382.0), o + f * (300.0, 400.0));
        end Bezier_Curves_Demo;
        y0 : Real := 600.0;
      begin
        pdf.Stroking_Color (black);
        pdf.Line_Width (initial_line_width);
        pdf.Font_Size (10.0);
        for rend in Path_Rendering_Mode loop
          for rule in Inside_path_rule loop
            for close_it in reverse Boolean loop
              pdf.Filling_Color ((0.2, 0.5, 1.0));
              Bezier_Curves_Demo ((0.0, y0 - 50.0));
              pdf.Finish_Path (close_it, rend, rule);
              pdf.Color (black);
              pdf.Put_XY (150.0, y0 + 70.0,
                "rend = " & Path_Rendering_Mode'Image (rend) &
                ", rule = " & Inside_path_rule'Image (rule) &
                ", close path = " & Boolean'Image (close_it)
              );
              y0 := y0 - 45.0;
            end loop;
          end loop;
        end loop;
      end;

      ------------------------------------------------------
      --  More advanced vector graphics - arc of circles  --
      ------------------------------------------------------
      pdf.New_Page;
      Arc_Demo;

      ------------------------
      --  The Ada mascot !  --
      ------------------------
      pdf.Finish_Page;  --  Needed for having the footer right before changing orientation.
      pdf.Page_Setup (A4_landscape);
      pdf.New_Page;
      pdf.Put ("Direct PDF vector graphics code inclusion - Ada Mascot");
      for cx in 1 .. 8 loop
        for cy in 1 .. 6 loop
          Insert_Mascot
            (pdf,
             w => Real (cx) * 17.0,
             h => Real (cy) * 17.0,
             xm => pdf.Left_Margin + Real (cx * (cx - 1) / 2) * 20.0,
             ym => pdf.Bottom_Margin + Real (cy * (cy - 1) / 2) * 20.0);
        end loop;
      end loop;
      ------------------------------------------------------------------
      --  Bar code from Ada Bar Codes - http://ada-bar-codes.sf.net/  --
      ------------------------------------------------------------------
      pdf.New_Page;
      Bar_Code_Demo;

      --
      --  Finishing
      --
      mem_page_nb := pdf.Page;
      pdf.Close;
    end Big_Demo_Contents;

  begin
    --  Dry run: here page_nb is properly set (TeX-style processing):
    Big_Demo_Contents ("Big.pdf");  --  Need a portable version of /dev/null (UNIX) or nul (Win.)
    --  Real file:
    Big_Demo_Contents ("Big.pdf");
  end Big_Demo;

  use Ada.Text_IO;

begin
  Put_Line ("Small demo -> Small.pdf");
  Small_Demo;
  Put_Line ("Big demo -> Big.pdf");
  Big_Demo;
end PDF_Out_Demo;
