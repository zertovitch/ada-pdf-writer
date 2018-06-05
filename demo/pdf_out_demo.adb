--  This test procedure for PDF_Out is in the Ada 95 syntax,
--  for compatibility with a larger number of development systems.
--  With Ada 2005 and later, you can also write "pdf.Write(...)" etc. everywhere.
--

with PDF_Out;                           use PDF_Out;
with Fancy_page;                        use Fancy_page;
with Insert_Mascot;

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

    procedure Big_demo_contents(name: String) is
      factor: Real;
      pdf: Fancy_PDF;
      bordu: constant String:= "demo/bordu_2016_01_01_25pct.jpg";
      target: Rectangle;
    begin
      pdf.page_nb:= mem_page_nb;
      Create(pdf, name);
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
      New_Line(pdf);
      Put_Line(pdf, "Testing Latin-1 characters: je répète, ganz schöne Umläute");
      New_Line(pdf, 2);
      Line_Spacing(pdf, 2.0);
      for r in 1..5 loop
        Color(pdf, (Real(r) * 0.19999, 0.0, 0.0));
        Put_Line(pdf, "Variations of red...");
      end loop;
      Line_Spacing(pdf, default_line_spacing);
      for n in 1..4 loop
        factor:= Real(n) * 0.03;
        target:= Get_pixel_dimensions(bordu);
        Image(pdf,
          bordu,
          (Left_Margin(pdf), Bottom_Margin(pdf)) +
          (Real(n)* 60.0, Real(n)* 100.0 - 50.0) +
           factor * target
        );
      end loop;
      Color(pdf, black);
      for x in -curve_max..curve_max loop
        Text_XY(pdf,
          Left_Margin(pdf) + Real(curve_max + x) * one_cm,
          Bottom_Margin(pdf) + (1.0 + (Real(x)*0.4) ** 2) * one_cm
        );
        Put(pdf, Integer'Image(x));
      end loop;
      New_Line(pdf);
      Finish_Page(pdf);  --  Needed for having the footer right before changing orientation.
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
        Boarding_pass(205.0, "ZERTE, JULES", z_from, z_to, z_date);
        Boarding_pass(  5.0, "ZERTE, ROMEOTTE", z_from, z_to, z_date);
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
      Finish_Page(pdf);  --  Needed for having the footer right before changing orientation.
      Page_Setup(pdf, A4_portrait);
      New_Page(pdf);
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
        Stroking_Color(pdf, black);
        Line_Width(pdf, initial_line_width);
        Font_Size(pdf, 10.0);
        for rend in Path_Rendering_Mode loop
          for rule in Inside_path_rule loop
            for close_it in reverse Boolean loop
              Color(pdf, (0.2,0.5,1.0));
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
      Finish_Page(pdf);  --  Needed for having the footer right before changing orientation.
      Page_Setup(pdf, A4_landscape);
      --
      --  The Ada mascot !
      --
      New_Page(pdf);
      Put(pdf, "Direct PDF vector graphics code inclusion - Ada Mascot: see http://joinadanow.com/#mascot");
      for cx in 1..8 loop
        for cy in 1..6 loop
          Insert_Mascot(
            pdf,
            w => Real(cx) * 17.0,
            h => Real(cy) * 17.0,
            xm => Left_Margin(pdf) + Real(cx*(cx-1)/2) * 20.0,
            ym => Bottom_Margin(pdf) + Real(cy*(cy-1)/2) * 20.0
          );
        end loop;
      end loop;
      --
      --  The Ada mascot !
      --
      New_Page(pdf);
      Put(pdf, "Direct PDF vector graphics code inclusion - a bar code from: http://ada-bar-codes.sf.net/");
      --
      --  The bar code generator can be called programmatically as well...
      --
      pdf.Insert_Graphics_PDF_Code(
        "%  Begin of Bar code                                                                                        " & ASCII.LF &
        "%    Automatically generated by Ada Bar Codes version 001, preview 1, (2018) @ http://ada-bar-codes.sf.net/ " & ASCII.LF &
        "%    Bar code format: CODE_128                                                                              " & ASCII.LF &
        "%    Text to be encoded: [Hello from Ada Bar Codes ( http://ada-bar-codes.sf.net/ ) !]                      " & ASCII.LF &
        "%    This PDF snippet has to be included into a PDF document.                                               " & ASCII.LF &
        "%    For instance, use Insert_Graphics_PDF_Code of PDF_Out, http://apdf.sf.net/                             " & ASCII.LF &
        "q                                                                                                           " & ASCII.LF &
        "0 g                                                                                                         " & ASCII.LF &
        "    100 200 1.7543859649122807 200 re                                                                       " & ASCII.LF &
        "    102.631578947368421 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    105.263157894736842 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    109.649122807017544 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    114.035087719298246 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    115.789473684210526 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    119.298245614035088 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    121.052631578947368 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    124.56140350877193 200 .877192982456140351 200 re                                                       " & ASCII.LF &
        "    128.947368421052632 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    132.456140350877193 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    134.210526315789474 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    138.596491228070175 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    142.105263157894737 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    143.859649122807018 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    148.245614035087719 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    151.754385964912281 200 3.5087719298245614 200 re                                                       " & ASCII.LF &
        "    156.140350877192982 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    157.894736842105263 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    160.526315789473684 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    164.035087719298246 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    167.543859649122807 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    169.298245614035088 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    174.56140350877193 200 .877192982456140351 200 re                                                       " & ASCII.LF &
        "    177.192982456140351 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    179.824561403508772 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    182.456140350877193 200 3.5087719298245614 200 re                                                       " & ASCII.LF &
        "    186.842105263157895 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    190.350877192982456 200 3.5087719298245614 200 re                                                       " & ASCII.LF &
        "    194.736842105263158 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    196.491228070175439 200 3.5087719298245614 200 re                                                       " & ASCII.LF &
        "    200.87719298245614 200 2.63157894736842105 200 re                                                       " & ASCII.LF &
        "    204.385964912280702 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    206.140350877192982 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    208.771929824561404 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    212.280701754385965 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    215.789473684210526 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    217.543859649122807 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    221.052631578947368 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    225.43859649122807 200 .877192982456140351 200 re                                                       " & ASCII.LF &
        "    229.824561403508772 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    232.456140350877193 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    235.087719298245614 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    237.719298245614035 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    239.473684210526316 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    244.736842105263158 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    247.368421052631579 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    250.87719298245614 200 1.7543859649122807 200 re                                                        " & ASCII.LF &
        "    254.385964912280702 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    257.894736842105263 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    259.649122807017544 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    264.035087719298246 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    266.666666666666667 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    268.421052631578947 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    273.684210526315789 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    276.315789473684211 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    278.947368421052632 200 3.5087719298245614 200 re                                                       " & ASCII.LF &
        "    283.333333333333333 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    285.964912280701754 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    289.473684210526316 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    292.982456140350877 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    296.491228070175439 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    300 200 1.7543859649122807 200 re                                                                       " & ASCII.LF &
        "    302.631578947368421 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    306.140350877192982 200 3.5087719298245614 200 re                                                       " & ASCII.LF &
        "    310.526315789473684 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    312.280701754385965 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    316.666666666666667 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    319.298245614035088 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    321.929824561403509 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    323.68421052631579 200 1.7543859649122807 200 re                                                        " & ASCII.LF &
        "    327.192982456140351 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    331.578947368421053 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    333.333333333333333 200 3.5087719298245614 200 re                                                       " & ASCII.LF &
        "    338.596491228070175 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    341.228070175438596 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    343.859649122807018 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    347.368421052631579 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    350.87719298245614 200 .877192982456140351 200 re                                                       " & ASCII.LF &
        "    354.385964912280702 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    357.894736842105263 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    360.526315789473684 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    363.157894736842105 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    366.666666666666667 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    370.175438596491228 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    372.807017543859649 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    378.070175438596491 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    379.824561403508772 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    382.456140350877193 200 3.5087719298245614 200 re                                                       " & ASCII.LF &
        "    386.842105263157895 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    389.473684210526316 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    392.105263157894737 200 3.5087719298245614 200 re                                                       " & ASCII.LF &
        "    396.491228070175439 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    399.12280701754386 200 .877192982456140351 200 re                                                       " & ASCII.LF &
        "    400.87719298245614 200 .877192982456140351 200 re                                                       " & ASCII.LF &
        "    403.508771929824561 200 3.5087719298245614 200 re                                                       " & ASCII.LF &
        "    408.771929824561404 200 2.63157894736842105 200 re                                                      " & ASCII.LF &
        "    413.157894736842105 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    415.789473684210526 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    418.421052631578947 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    420.175438596491228 200 2.63157894736842105 200 re                                                      " & ASCII.LF &
        "    424.56140350877193 200 1.7543859649122807 200 re                                                        " & ASCII.LF &
        "    428.070175438596491 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    429.824561403508772 200 2.63157894736842105 200 re                                                      " & ASCII.LF &
        "    434.210526315789474 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    437.719298245614035 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    440.350877192982456 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    442.105263157894737 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    447.368421052631579 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    451.754385964912281 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    454.385964912280702 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    457.017543859649123 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    459.649122807017544 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    461.403508771929825 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    466.666666666666667 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    469.298245614035088 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    471.929824561403509 200 2.63157894736842105 200 re                                                      " & ASCII.LF &
        "    476.315789473684211 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    478.947368421052632 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    483.333333333333333 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    485.964912280701754 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    488.596491228070175 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    490.350877192982456 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    495.614035087719298 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    498.245614035087719 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    500.87719298245614 200 3.5087719298245614 200 re                                                        " & ASCII.LF &
        "    505.263157894736842 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    507.894736842105263 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    510.526315789473684 200 2.63157894736842105 200 re                                                      " & ASCII.LF &
        "    514.912280701754386 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    519.298245614035088 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    521.052631578947368 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    524.56140350877193 200 .877192982456140351 200 re                                                       " & ASCII.LF &
        "    528.070175438596491 200 3.5087719298245614 200 re                                                       " & ASCII.LF &
        "    532.456140350877193 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    534.210526315789474 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    538.596491228070175 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    541.228070175438596 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    543.859649122807018 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    545.614035087719298 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    549.12280701754386 200 .877192982456140351 200 re                                                       " & ASCII.LF &
        "    553.508771929824561 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    555.263157894736842 200 3.5087719298245614 200 re                                                       " & ASCII.LF &
        "    560.526315789473684 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    563.157894736842105 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    565.789473684210526 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    569.298245614035088 200 2.63157894736842105 200 re                                                      " & ASCII.LF &
        "    572.807017543859649 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    574.56140350877193 200 3.5087719298245614 200 re                                                        " & ASCII.LF &
        "    579.824561403508772 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    582.456140350877193 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    584.210526315789474 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    589.473684210526316 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    592.105263157894737 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    594.736842105263158 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    598.245614035087719 200 2.63157894736842105 200 re                                                      " & ASCII.LF &
        "    601.754385964912281 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    607.017543859649123 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    608.771929824561404 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    611.403508771929825 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    613.157894736842105 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    616.666666666666667 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    621.052631578947368 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    623.68421052631579 200 3.5087719298245614 200 re                                                        " & ASCII.LF &
        "    628.070175438596491 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    630.701754385964912 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    632.456140350877193 200 2.63157894736842105 200 re                                                      " & ASCII.LF &
        "    636.842105263157895 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    640.350877192982456 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    642.982456140350877 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    646.491228070175439 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    650 200 1.7543859649122807 200 re                                                                       " & ASCII.LF &
        "    653.508771929824561 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    656.140350877192982 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    659.649122807017544 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    662.280701754385965 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    665.789473684210526 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    669.298245614035088 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    672.807017543859649 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    675.43859649122807 200 1.7543859649122807 200 re                                                        " & ASCII.LF &
        "    678.947368421052632 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    682.456140350877193 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    685.964912280701754 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    688.596491228070175 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "    692.982456140350877 200 2.63157894736842105 200 re                                                      " & ASCII.LF &
        "    696.491228070175439 200 .877192982456140351 200 re                                                      " & ASCII.LF &
        "    698.245614035087719 200 1.7543859649122807 200 re                                                       " & ASCII.LF &
        "Q                                                                                                           " & ASCII.LF &
        "%  End of bar code"
      );
      --
      --  Finishing
      --
      mem_page_nb:= Page(pdf);
      Close(pdf);
    end Big_demo_contents;

  begin
    --  Dry run: here page_nb is properly set (TeX-style processing):
    Big_demo_contents("Big.pdf");  --  Need a portable version of /dev/null (UNIX) or nul (Win.)
    --  Real file:
    Big_demo_contents("Big.pdf");
  end Big_demo;

  use Ada.Text_IO;

begin
  Put_Line("Small demo -> Small.pdf");
  Small_demo;
  Put_Line("Big demo -> Big.pdf");
  Big_demo;
end PDF_Out_Demo;
