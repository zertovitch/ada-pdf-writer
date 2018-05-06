with Ada.Text_IO; use Ada.Text_IO;
with PDF_Out; use PDF_Out;

procedure Page_test is
  pdf: PDF_Out_File;
begin
  pdf.Create ("Page.pdf");
  Put_Line ("Page # is" & Integer'Image(pdf.Page) & " - after creation");
  for i in 1 .. 10 loop
    pdf.Put_Line ("This page is..." & Integer'Image(i));
    Put_Line ("Page # is" & Integer'Image(pdf.Page) & " - after an output");
    pdf.Put_Line ("PDF_Out says:" & Integer'Image(pdf.Page));
    pdf.New_Page;
    Put_Line ("Page # is" & Integer'Image(pdf.Page) & " - after New_Page");
  end loop;
  pdf.Put_Line ("This is the last page." & Integer'Image(pdf.Page));
  pdf.Close;
end;
