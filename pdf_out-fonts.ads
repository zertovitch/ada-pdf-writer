private package PDF_Out.Fonts is

  function Standard_Font_Name (f: Standard_Font_Type) return String;

  --  Font dictionary name within a page
  --
  function Font_Dictionary_Name (font_name: String) return String;

  --  Font dictionary name within a page, for standard fonts
  --  Example: /Ada_PDF_Std_Font_Courier-Oblique
  --
  function Standard_Font_Dictionary_Name (f: Standard_Font_Type) return String;

  --  Output font dictionary (resource for last page)
  --
  procedure Font_Dictionary (pdf: in out PDF_Out_Stream'Class);

  function Current_Font_Name (pdf: PDF_Out_Stream'Class) return String;

  function Current_Font_Dictionary_Name (pdf: PDF_Out_Stream'Class) return String;

end PDF_Out.Fonts;
