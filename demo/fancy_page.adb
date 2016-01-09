package body Fancy_page is

  pt_font: constant:= 40.0;

  use PDF_Out;

  procedure Page_Header (pdf : in out Fancy_PDF) is
    pg_layout: constant PDF_Out.Rectangle:= Layout(pdf);
    factor: constant:= 1.0;
    ada_logo_width  : constant:= 140.0 * factor;
    ada_logo_height : constant:= 78.0 * factor;
  begin
    Text_XY(pdf, Left_Margin(pdf), Y_Max(pg_layout) - Top_Margin(pdf) * 0.2 - pt_font * 0.5);
    Text_Rendering_Mode(pdf, fill_then_stroke);
    Font(pdf, Helvetica);
    Font_Size(pdf, 18.7);
    Stroking_Color(pdf, (0.8,0.1,0.0));
    Color(pdf, (0.7,0.7,0.0));
    Put(pdf, "Fancy header");  --  Red outline, yellow fill
    Color(pdf, black);
    Stroking_Color(pdf, black);
    Put(pdf, " for ");
    Stroking_Color(pdf, (0.0,0.5,0.1));
    Color(pdf, (0.0,0.3,0.3));
    Put(pdf, "Ada PDF Writer Demo");  --  Green outline, cyan fill
    Image(pdf,
      "demo/ada_logo.jpg",
      (X_Max(pg_layout) - ada_logo_width,
       Y_Max(pg_layout) - ada_logo_height,
       ada_logo_width,
       ada_logo_height)
    );
    Stroking_Color(pdf, (0.0,0.0,0.3));  --  Dark blue
    Color(pdf, (0.0,0.1,0.6));  --  A bit less dark blue
    Draw(pdf,
      (Left_Margin(pdf),
       Y_Max(pg_layout) - Top_Margin(pdf) + pt_font,
       X_Max(pg_layout) - ada_logo_width - Left_Margin(pdf),
       one_cm * 0.2),
       fill_then_stroke
    );
    --
    --  Back to normal
    --
    Text_Rendering_Mode(pdf, fill);
    Color(pdf, black);
    Stroking_Color(pdf, black);
    Font_Size(pdf, 11.0);
  end Page_Header;

  procedure Page_Footer (pdf : in out Fancy_PDF) is
    pg_layout: constant PDF_Out.Rectangle:= Layout(pdf);
  begin
    Color(pdf, black);
    Text_XY(pdf, Left_Margin(pdf), pg_layout.y_min + Bottom_Margin(pdf) - pt_font);
    Put(pdf, "Page" & Integer'Image(Page(pdf)) & " /" & Integer'Image(pdf.page_nb));
  end Page_Footer;

end Fancy_page;
