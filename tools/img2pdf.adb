--  This tool creates a PDF including one or more images files.
--  The file names are given as command-line parameters.

with PDF_Out;                           use PDF_Out;

with Ada.Command_Line;                  use Ada.Command_Line;

procedure Img2PDF is
  --
  --  Hardcoded scan and page parameters:
  --
  DPI  : constant := 300.0;  --  DPI = Dots (pixels) Per Inch
  page : constant Rectangle := A4_portrait;
  --
  left, bottom: Real;
  target: Rectangle;
  pdf: PDF_Out.PDF_Out_File;
begin
  Create(pdf, "img2pdf_out.pdf");
  Creator_Application(pdf, "Img2PDF");
  Page_Setup(pdf, page);
  for i in 1 .. Argument_Count loop
    --  Get image dimensions and look for a good fit into the page:
    target:= (one_inch / DPI) * Get_pixel_dimensions(Argument(i));
    left:= Left_Margin(pdf);
    if left + target.width > page.width then
      left:= 0.0;
    end if;
    bottom:= Bottom_Margin(pdf);
    if bottom + target.height > page.height then
      bottom:= 0.0;
    end if;
    --  Insert image:
    Image(pdf, Argument(i), (left, bottom) + target);
    if i < Argument_Count then
      New_Page(pdf);
    end if;
  end loop;
  Close(pdf);
end;
