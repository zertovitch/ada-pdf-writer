with PDF_Out;

procedure Coord_Test is
  use PDF_Out;

  pdf : PDF_Out_File;

begin
  pdf.Create ("coords.pdf");
  pdf.Page_Setup (A4_portrait);

  pdf.Draw
    ((pdf.Left_Margin,
      pdf.Bottom_Margin,
      pdf.Layout.width  - pdf.Right_Margin - pdf.Left_Margin,
      pdf.Layout.height - pdf.Top_Margin   - pdf.Bottom_Margin),
     stroke);

  pdf.New_Page;

  pdf.Set_Math_Plane ((0.0, 0.0, 1.0, 1.0));

  --  The second rectangle on the second page must look identical.

  pdf.Draw
    ((0.0,
      0.0,
      1.0,
      1.0),
     stroke);

  pdf.Close;
end Coord_Test;
