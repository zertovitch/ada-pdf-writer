with PDF_Out;

procedure Coord_Test is
  use PDF_Out;

  pdf : PDF_Out_File;

  procedure Test_Math_Plane (math_plane : Rectangle) is
  begin
    pdf.Set_Math_Plane (math_plane);
    --  The following rectangle must look identical to the first rectangle.
    pdf.Draw (math_plane, stroke);
  end Test_Math_Plane;

  custom : constant Rectangle :=
    (pdf.Left_Margin + one_inch, pdf.Bottom_Margin, one_inch, 3.0 * one_inch);

  procedure Test_Math_Plane_Custom_Target (math_plane : Rectangle) is
  begin
    pdf.Set_Math_Plane (math_plane, custom);
    --  The following rectangle must look identical to the rectangle in "paper" coordinates.
    pdf.Draw (math_plane, stroke);
  end Test_Math_Plane_Custom_Target;

begin
  pdf.Create ("coords.pdf");
  pdf.Page_Setup (A4_portrait);

  -----------------------------------------------------------
  --  Test with the default target area, the page margins  --
  -----------------------------------------------------------

  --  Draw a rectangle along the page margins:

  pdf.Draw
    ((pdf.Left_Margin,
      pdf.Bottom_Margin,
      pdf.Layout.width  - pdf.Right_Margin - pdf.Left_Margin,
      pdf.Layout.height - pdf.Top_Margin   - pdf.Bottom_Margin),
     stroke);

  pdf.New_Page;

  Test_Math_Plane ((0.0, 0.0, 1.0, 1.0));  --  The "academic square"

  pdf.New_Page;

  Test_Math_Plane ((-7.13, 6.23, 67.2, 0.057));  --  Some fuzzy numbers

  pdf.New_Page;

  pdf.Restore_Paper_Plane;

  --------------------------------------
  --  Test with a custom target area  --
  --------------------------------------

  --  Draw a custom rectangle in "paper" coordinates (Points):

  pdf.Draw (custom, stroke);

  pdf.New_Page;

  Test_Math_Plane_Custom_Target ((-3.1, -7.2, 53.8, 24.0));

  pdf.New_Page;

  Test_Math_Plane_Custom_Target ((8.3, 77.77, 3.18, 1.9));

  pdf.Close;
end Coord_Test;
