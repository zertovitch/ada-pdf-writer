PDF Writer 
==========

The PDF Writer consists of a package, PDF_Out,
which produces PDF files - as "physical" files, or as
other types of data streams.

The creation of a PDF file is as simple as this
small procedure:


  with PDF_Out;

  procedure Hello is
    pdf : PDF_Out.PDF_Out_File;
  begin
    pdf.Create ("tiny.pdf");
    pdf.Put_Line ("Hello world !");
    pdf.Close;
  end;


====

Full description in: pdf_writer.txt
