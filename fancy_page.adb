package body Fancy_page is

  pt_font: constant:= 40.0;

  use PDF_Out;

  procedure Page_Header (pdf : in out Fancy_PDF) is
    pg_layout: constant PDF_Out.Rectangle:= Layout(pdf);
    factor: constant:= 1.0;
    al_width  : constant:= 140.0 * factor;
    al_height : constant:= 78.0 * factor;
  begin
    Text_XY(pdf, Left_Margin(pdf), Y_Max(pg_layout) - Top_Margin(pdf) + pt_font);
    Rendering_Mode(pdf, fill_then_stroke);
    Insert_PDF_Code(pdf, "/F1 20 Tf"); -- !! to be changed for proper font selector
    Color(pdf, (0.9,0.9,0.0));
    Stroking_Color(pdf, (0.6,0.0,0.0));
    Put(pdf, "Fancy header");  --  Red outline, yellow fill
    Image(pdf,
      "ada_logo.jpg",
      (X_Max(pg_layout) - al_width, Y_Max(pg_layout) - al_height, al_width, al_height)
    );
    --
    --  Back to normal
    --
    Rendering_Mode(pdf, fill);
    Color(pdf, black);
    Insert_PDF_Code(pdf, "/F1 11 Tf"); -- !! to be changed for proper font selector
  end Page_Header;

  procedure Page_Footer (pdf : in out Fancy_PDF) is
    pg_layout: constant PDF_Out.Rectangle:= Layout(pdf);
  begin
    Text_XY(pdf, Left_Margin(pdf), pg_layout.y_min + Bottom_Margin(pdf) - pt_font);
    Put(pdf, "Page" & Integer'Image(Page(pdf)) & " /" & Integer'Image(pdf.page_nb));
  end Page_Footer;

end Fancy_page;
