package body Fancy_page is

  pt_font: constant:= 25.0;

  procedure Page_Header (pdf : in out Fancy_PDF) is
    pg_layout: constant PDF_Out.Rectangle:= Layout(pdf);
  begin
    Text_XY(pdf, Left_Margin(pdf), pg_layout.y_max - Top_Margin(pdf) + pt_font);
    Put(pdf, "Fancy header");
  end Page_Header;

  procedure Page_Footer (pdf : in out Fancy_PDF) is
    pg_layout: constant PDF_Out.Rectangle:= Layout(pdf);
  begin
    Text_XY(pdf, Left_Margin(pdf), pg_layout.y_min + Bottom_Margin(pdf) - pt_font);
    Put(pdf, "Page" & Integer'Image(Page(pdf)) & " /" & Integer'Image(pdf.page_nb));
  end Page_Footer;

end Fancy_page;
