with PDF_Out;

with Ada.Numerics;

procedure Arc_Test is
  use PDF_Out;

  pdf : PDF_Out_File;

  procedure Test_Various_Angles_and_Modes is
    scale : constant := 20.0;
    some_angle : constant := 135.0;
    angle_1 : Real;
    center : Point;
  begin
    pdf.Stroking_Color ((0.3, 0.1, 0.0));  --  Dark red
    pdf.Filling_Color  ((0.6, 1.0, 0.6));  --  Light green

    for non_trivial_angle_1 in 0 .. 1 loop
      for rend in stroke .. fill_then_stroke loop
        for is_pie in Boolean loop
          for n in 1 .. 10 loop
            center :=
              (0.0, pdf.Layout.height - pdf.Margins.top) +
              scale * 3.0 *
                (Real
                  (1 + Boolean'Pos (is_pie) +
                   2 * (Rendering_Mode'Pos (rend) - Rendering_Mode'Pos (stroke)) +
                   4 * non_trivial_angle_1),
                 -Real (n));
            if is_pie then
              pdf.Move (center);
            end if;
            angle_1 := Real (non_trivial_angle_1) * some_angle;
            Arc (pdf, center, scale, angle_1, angle_1 + Real (n) * 36.0, is_pie);
            if is_pie then
              pdf.Line (center);
            end if;
            pdf.Finish_Path (False, rend, nonzero_winding_number);
          end loop;
        end loop;
      end loop;
    end loop;
  end Test_Various_Angles_and_Modes;

  procedure Test_Pie_Charts is

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
        Move (pdf, shift + center);
        Filling_Color (pdf, p (i).color);
        Arc (pdf, shift + center, radius, angle_1, angle_2, True);
        angle_1 := angle_2;
        Line (pdf, shift + center);
        Finish_Path (pdf, False, fill_then_stroke, nonzero_winding_number);
      end loop;
    end Pie;

    radius : constant := 60.0;

  begin
    pdf.Stroking_Color ((0.25, 0.0, 0.0));
    pdf.Line_Width (2.0);
    for pull in pull_apart .. 3 loop
      Pie
        ((pdf.Margins.left + radius,
          pdf.Layout.height - pdf.Margins.top - radius * (1.0 + Real (1 + pull) * 2.5)),
         radius,
         90.0,
         pie_1,
         pull);
    end loop;
  end Test_Pie_Charts;

begin
  pdf.Create ("arcs.pdf");
  pdf.Page_Setup (A4_portrait);

  Test_Pie_Charts;

  pdf.New_Page;

  Test_Various_Angles_and_Modes;

  pdf.Close;
end Arc_Test;
