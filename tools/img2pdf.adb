with PDF_Out;                           use PDF_Out;

with Ada.Command_Line;                  use Ada.Command_Line;

procedure Img2PDF is
  factor: Real;
  DPI: constant:= 300.0;
  target: Rectangle;
  pdf: PDF_Out.PDF_Out_File;
begin
  Create(pdf, "out.pdf");
  Creator_Application(pdf, "Img2PDF");
  for i in 1..Argument_Count loop
    factor:= one_inch / DPI;
    target:= Get_pixel_dimensions(Argument(i));
    Image(pdf, Argument(i), (Left_Margin(pdf), Bottom_Margin(pdf)) + factor * target);
    if i < Argument_Count then
      New_Page(pdf);
    end if;
  end loop;
  Close(pdf);
end;
