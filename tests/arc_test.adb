with PDF_Out;

with Ada.Numerics.Generic_Elementary_Functions;

procedure Arc_Test is
  use PDF_Out;

  function Almost_Zero (x : Real) return Boolean is
  begin
    return abs x <= Real'Base'Model_Small;
  end Almost_Zero;

  --  8.2

  procedure Arc
    (pdf              : in out PDF_Out_Stream'Class;
     center           :        Point;
     r                :        Real;
     angle_1, angle_2 :        Real;
     line_to_start    :        Boolean)
  is
    package REF is new Ada.Numerics.Generic_Elementary_Functions (Real);
    use Ada.Numerics, REF;
    sweep_angle : Real;
    n_curves : Integer;
    deg_to_rad  : constant := Pi / 180.0;
    angle_start : constant Real := angle_1 * deg_to_rad;
    angle_stop  : constant Real := angle_2 * deg_to_rad;
    sn, cs, sweep_angle_part, angle_part_start : Real;
    p, q : array (0 .. 3) of Point;
    v, t, q0 : Point;
    scale : constant := 4.0 / 3.0;  --  This value is a bit magic.
  begin
    sweep_angle := angle_stop - angle_start;

    if Almost_Zero (sweep_angle) then
      return;
    end if;

    --  If sweep_angle is too large, divide arc to smaller ones:
    n_curves := Integer (Real'Ceiling (abs (sweep_angle) / (Pi * 0.5)));

    sweep_angle_part := sweep_angle / Real (n_curves);

    --  Calculates control points for Bezier approximation of
    --  an arc with radius=1, circle center at (0,0),
    --  middle of the arc at (1,0).

    v.x := Cos (sweep_angle_part * 0.5);
    v.y := Sin (sweep_angle_part * 0.5);

    t.x := (1.0 - v.x) * scale;
    t.y := -t.x * v.x / v.y;  --  Vectors t and v must be perpendicular.

    --  End point 2
    p (3) := v;
    --  Control point 2
    p (2) := v + t;
    --  Control point 1
    p (1) := (+p (2).x, -p (2).y);
    --  End point 1
    p (0) := (+p (3).x, -p (3).y);

    --  Rotation and translation of control points,
    --  the start point and the end points.

    cs := Cos (angle_start + sweep_angle_part * 0.5);
    sn := Sin (angle_start + sweep_angle_part * 0.5);

    q0 := center + r * (p (0).x * cs - p (0).y * sn,
                        p (0).x * sn + p (0).y * cs);

    if line_to_start then
      Line (pdf, q0);
    else
      Move (pdf, q0);
    end if;

    for i_curve in 1 .. n_curves loop
      angle_part_start := angle_start + sweep_angle_part * Real (i_curve - 1);

      sn := Sin (angle_part_start + sweep_angle_part * 0.5);
      cs := Cos (angle_part_start + sweep_angle_part * 0.5);

      for i in 1 .. 3 loop
        q (i) :=
          center + r * (p (i).x * cs - p (i).y * sn,
                        p (i).x * sn + p (i).y * cs);
      end loop;

      Cubic_Bezier (pdf, q (1), q (2), q (3));
    end loop;
  end Arc;

  pdf : PDF_Out_File;

  scale : constant := 20.0;
  some_angle : constant := 135.0;
  angle_1 : Real;
  center : Point;

begin
  pdf.Create ("arc.pdf");

  pdf.Page_Setup (A4_portrait);
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

  pdf.Close;
end Arc_Test;
