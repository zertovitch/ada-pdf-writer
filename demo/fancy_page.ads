--  Example of a customized PDF page style.
--  Customization is done overriding methods (e.g. header, footer) in PDF_Out.

with PDF_Out;

package Fancy_Page is

  type Fancy_PDF is new PDF_Out.PDF_Out_File with record
    --  Total page number, obtained by running twice (TeX-fashion):
    page_nb : Natural := 0;
  end record;

  overriding procedure Page_Header (pdf : in out Fancy_PDF);
  overriding procedure Page_Footer (pdf : in out Fancy_PDF);

end Fancy_Page;
