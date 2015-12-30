--  Demo for custom, overriden methods (e.g. header, footer) in PDF_Out

with PDF_Out;

package Fancy_page is

  type Fancy_PDF is new PDF_Out.PDF_Out_String with record
    page_nb: Natural:= 0;  --  Total page number, obtained by running twice (à la TeX).
  end record;

  procedure Page_Header(pdf : in out Fancy_PDF);
  procedure Page_Footer(pdf : in out Fancy_PDF);

end Fancy_page;
