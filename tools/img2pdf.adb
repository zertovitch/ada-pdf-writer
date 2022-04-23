--  This tool creates a PDF including one or more images files.
--  The file names are given as command-line parameters.

with PDF_Out;

with Ada.Command_Line;

procedure Img2PDF is
  use Ada.Command_Line, PDF_Out;
  --
  --  Hardcoded scan and page parameters:
  --
  DPI  : constant := 300.0;  --  DPI = Dots (pixels) Per Inch
  page : constant Rectangle := A4_portrait;
  --
  left, bottom : Real;
  target : Rectangle;
  pdf : PDF_Out_File;
begin
  pdf.Create ("img2pdf_out.pdf");
  pdf.Creator_Application ("Img2PDF");
  pdf.Page_Setup (page);
  for i in 1 .. Argument_Count loop
    --  Get image dimensions and look for a good fit into the page:
    target := (one_inch / DPI) * Get_pixel_dimensions (Argument (i));
    left := pdf.Left_Margin;
    if left + target.width > page.width then
      left := 0.0;
    end if;
    bottom := pdf.Bottom_Margin;
    if bottom + target.height > page.height then
      bottom := 0.0;
    end if;
    --  Insert image:
    pdf.Image (Argument (i), (left, bottom) + target);
    if i < Argument_Count then
      pdf.New_Page;
    end if;
  end loop;
  pdf.Close;
end Img2PDF;
