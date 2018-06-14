package body PDF_Out.Fonts is

  --  9.6.2.2  Standard Type 1 Fonts (Standard 14 Fonts)
  function Standard_Font_Name (f: Standard_Font_Type) return String is
  begin  --  Code generation: see pw_work.xls, Fonts (Std)
    case f is
      when Courier                 => return "Courier";
      when Courier_Bold            => return "Courier-Bold";
      when Courier_Bold_Oblique    => return "Courier-BoldOblique";
      when Courier_Oblique         => return "Courier-Oblique";
      when Helvetica               => return "Helvetica";
      when Helvetica_Bold          => return "Helvetica-Bold";
      when Helvetica_Bold_Oblique  => return "Helvetica-BoldOblique";
      when Helvetica_Oblique       => return "Helvetica-Oblique";
      when Symbol                  => return "Symbol";
      when Times_Bold              => return "Times-Bold";
      when Times_Bold_Italic       => return "Times-BoldItalic";
      when Times_Italic            => return "Times-Italic";
      when Times_Roman             => return "Times-Roman";
      when Zapf_Dingbats           => return "ZapfDingbats";
    end case;
  end Standard_Font_Name;

  function Font_Dictionary_Name (font_name: String) return String is
  begin
    return "/Ada_PDF_Font_" & font_name;
  end Font_Dictionary_Name;

  function Standard_Font_Dictionary_Name (f: Standard_Font_Type) return String is
  begin
    return "/Ada_PDF_Std_Font_" & Standard_Font_Name(f);
  end Standard_Font_Dictionary_Name;

  --  7.8.3 Resource Dictionaries (any resources required by a page).
  --  Table 33: Font: A dictionary that maps resource names to font dictionaries.
  --
  procedure Font_Dictionary (pdf: in out PDF_Out_Stream'Class) is
  begin
    WL(pdf, "  /Font <<");  --  font dictionary
    for f in Standard_Font_Type loop
      WL(pdf,
        "    " & Standard_Font_Dictionary_Name(f) &
        " << /Type /Font /Subtype /Type1 /BaseFont /" & Standard_Font_Name(f) &
        --  7.9.2.2 Text String Type: "PDFDocEncoding can encode all of
        --  the ISO Latin 1 character set and is documented in Annex D."
        --  PDFDocEncoding is recognized by the Chrome PDF viewer on Windows but...
        --  *isn't* by Adobe Reader X, on Windows! So we resort to another ISO
        --  Latin 1 superset: WinAnsiEncoding = Windows Code Page 1252 (Table D.1).
        " /Encoding /WinAnsiEncoding " &
        " >>"
      );
    end loop;
    WL(pdf, "    >>");
  end Font_Dictionary;

  function Current_Font_Name (pdf: PDF_Out_Stream'Class) return String is
  begin
    if pdf.current_font in Standard_Font_Type then
      return Standard_Font_Name(pdf.current_font);
    else
      return To_String(pdf.ext_font_name);
    end if;
  end Current_Font_Name;

  function Current_Font_Dictionary_Name (pdf: PDF_Out_Stream'Class) return String is
  begin
    if pdf.current_font in Standard_Font_Type then
      return Standard_Font_Dictionary_Name (pdf.current_font);
    else
      return Font_Dictionary_Name (To_String(pdf.ext_font_name));
    end if;
  end Current_Font_Dictionary_Name;

end PDF_Out.Fonts;
