package body Fancy_Page is

  pt_font : constant := 40.0;

  use PDF_Out;

  overriding procedure Page_Header (pdf : in out Fancy_PDF) is
    pg_layout : constant PDF_Out.Rectangle := pdf.Layout;
    factor : constant := 1.0;
    ada_logo_width  : constant := 140.0 * factor;
    ada_logo_height : constant := 78.0 * factor;
  begin
    pdf.Text_XY (pdf.Left_Margin, Y_Max (pg_layout) - pdf.Top_Margin * 0.2 - pt_font * 0.5);
    pdf.Text_Rendering_Mode (fill_then_stroke);
    pdf.Font (Helvetica);
    pdf.Font_Size (18.7);
    pdf.Stroking_Color ((0.8, 0.1, 0.0));
    pdf.Color ((0.7, 0.7, 0.0));
    pdf.Put ("Fancy header");  --  Red outline, yellow fill
    pdf.Color (black);
    pdf.Stroking_Color (black);
    pdf.Put (" for ");
    pdf.Stroking_Color ((0.0, 0.5, 0.1));
    pdf.Color ((0.0, 0.3, 0.3));
    pdf.Put ("Ada PDF Writer Demo");  --  Green outline, cyan fill
    pdf.Image (
      "demos/ada_logo.jpg",
      (X_Max (pg_layout) - ada_logo_width,
       Y_Max (pg_layout) - ada_logo_height,
       ada_logo_width,
       ada_logo_height)
    );
    pdf.Stroking_Color ((0.0, 0.0, 0.3));  --  Dark blue
    pdf.Color ((0.0, 0.1, 0.6));  --  A bit less dark blue
    pdf.Draw (
      (pdf.Left_Margin,
       Y_Max (pg_layout) - pdf.Top_Margin + pt_font,
       X_Max (pg_layout) - ada_logo_width - pdf.Left_Margin,
       one_cm * 0.2),
       fill_then_stroke
    );
    --
    --  Back to normal
    --
    pdf.Text_Rendering_Mode (fill);
    pdf.Color (black);
    pdf.Stroking_Color (black);
    pdf.Font_Size (11.0);
  end Page_Header;

  overriding procedure Page_Footer (pdf : in out Fancy_PDF) is
    pg_layout : constant PDF_Out.Rectangle := pdf.Layout;
  begin
    --  Footer line through the page:
    pdf.Line_Width (1.0);
    pdf.Stroking_Color (black);
    pdf.Single_Line (
      (pg_layout.x_min + pdf.Left_Margin,
       pg_layout.y_min + pdf.Bottom_Margin),
      (X_Max (pg_layout) - pdf.Right_Margin,
       pg_layout.y_min + pdf.Bottom_Margin)
    );
    --  Page numbering:
    pdf.Color (black);
    pdf.Font (Helvetica);
    pdf.Font_Size (11.0);
    pdf.Text_XY (pdf.Left_Margin, pg_layout.y_min + pdf.Bottom_Margin - pt_font);
    pdf.Put ("Page" & Integer'Image (pdf.Page) & " /" & Integer'Image (pdf.page_nb));
  end Page_Footer;

end Fancy_Page;
