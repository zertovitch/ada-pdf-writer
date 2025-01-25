with PDF_Out;

procedure Arc_Test is
  use PDF_Out;

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
