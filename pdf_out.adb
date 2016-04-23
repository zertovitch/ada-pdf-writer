with PDF_Out.Images;

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with Interfaces;                        use Interfaces;

package body PDF_Out is

  use Ada.Streams.Stream_IO, Ada.Streams;

  package CIO is new Ada.Text_IO.Integer_IO(Ada.Streams.Stream_IO.Count);

  -- Very low level part which deals with transferring data endian-proof,
  -- and floats in the IEEE format. This is needed for having PDF Writer
  -- totally portable on all systems and processor architectures.

  type Byte_buffer is array (Integer range <>) of Unsigned_8;

  -- Put numbers with correct endianess as bytes:
  generic
    type Number is mod <>;
    size: Positive;
  function Intel_x86_buffer( n: Number ) return Byte_buffer;
  pragma Inline(Intel_x86_buffer);

  function Intel_x86_buffer( n: Number ) return Byte_buffer is
    b: Byte_buffer(1..size);
    m: Number:= n;
  begin
    for i in b'Range loop
      b(i):= Unsigned_8(m and 255);
      m:= m / 256;
    end loop;
    return b;
  end Intel_x86_buffer;

  function Intel_32 is new Intel_x86_buffer( Unsigned_32, 4 );
  pragma Unreferenced (Intel_32);

  function Intel_16( n: Unsigned_16 ) return Byte_buffer is
    pragma Inline(Intel_16);
  begin
    return (Unsigned_8(n and 255), Unsigned_8(Shift_Right(n, 8)));
  end Intel_16;
  pragma Unreferenced (Intel_16);

  -- Workaround for the severe xxx'Read xxx'Write performance
  -- problems in the GNAT and ObjectAda compilers (as in 2009)
  -- This is possible if and only if Byte = Stream_Element and
  -- arrays types are both packed and aligned the same way.
  --
  subtype Size_test_a is Byte_buffer(1..19);
  subtype Size_test_b is Ada.Streams.Stream_Element_Array(1..19);
  workaround_possible: constant Boolean:=
    Size_test_a'Size = Size_test_b'Size and
    Size_test_a'Alignment = Size_test_b'Alignment;

  procedure Block_Read(
    file         : in     Ada.Streams.Stream_IO.File_Type;
    buffer       :    out Byte_buffer;
    actually_read:    out Natural
  )
  is
    SE_Buffer   : Stream_Element_Array (1 .. buffer'Length);
    for SE_Buffer'Address use buffer'Address;
    pragma Import (Ada, SE_Buffer);
    Last_Read   : Stream_Element_Offset;
  begin
    if workaround_possible then
      Read(Stream(file).all, SE_Buffer, Last_Read);
      actually_read:= Natural(Last_Read);
    else
      if End_Of_File(file) then
        actually_read:= 0;
      else
        actually_read:=
          Integer'Min( buffer'Length, Integer(Size(file) - Index(file) + 1) );
        Byte_buffer'Read(
          Stream(file),
          buffer(buffer'First .. buffer'First + actually_read - 1)
        );
      end if;
    end if;
  end Block_Read;

  procedure Block_Write(
    stream : in out Ada.Streams.Root_Stream_Type'Class;
    buffer : in     Byte_buffer
  )
  is
    pragma Inline(Block_Write);
    SE_Buffer   : Stream_Element_Array (1 .. buffer'Length);
    for SE_Buffer'Address use buffer'Address;
    pragma Import (Ada, SE_Buffer);
  begin
    if workaround_possible then
      Ada.Streams.Write(stream, SE_Buffer);
    else
      Byte_buffer'Write(stream'Access, buffer);
      -- ^ This was 30x to 70x slower on GNAT 2009
      --   Test in the Zip-Ada project.
    end if;
  end Block_Write;

  -- Copy a whole file into a stream, using a temporary buffer
  procedure Copy_file(
    file_name  : String;
    into       : in out Ada.Streams.Root_Stream_Type'Class;
    buffer_size: Positive:= 1024*1024
  )
  is
    f: File_Type;
    buf: Byte_buffer(1..buffer_size);
    actually_read: Natural;
  begin
    Open(f, In_File, file_name);
    loop
      Block_Read(f, buf, actually_read);
      exit when actually_read = 0; -- this is expected
      Block_Write(into, buf(1..actually_read));
    end loop;
    Close(f);
  end Copy_file;

  procedure W(pdf : in out PDF_Out_Stream'Class; s: String) is
  pragma Inline(W);
  begin
    String'Write(pdf.pdf_stream, s);
  end;

  NL: constant Character:= ASCII.LF;

  procedure WL(pdf : in out PDF_Out_Stream'Class; s: String) is
  begin
    W(pdf, s & NL);
  end;

  --  Delayed output, for internal PDF's "stream" object

  procedure Wd(pdf : in out PDF_Out_Stream'Class; s: String) is
  pragma Inline(Wd);
  begin
    if pdf.zone = nowhere then
      New_Page(pdf);
    end if;
    Append(pdf.stream_obj_buf, s);
  end;

  procedure WLd(pdf : in out PDF_Out_Stream'Class; s: String) is
  pragma Inline(WLd);
  begin
    Wd(pdf, s & NL);
  end;

  --  External stream index

  function Buffer_index(pdf : PDF_Out_Stream'Class) return Ada.Streams.Stream_IO.Count is
  begin
    return Index(pdf) - pdf.start_index;
  end Buffer_index;

  function Img(p: Integer) return String is
    s: constant String:= Integer'Image(p);
  begin
    if p < 0 then
      return s;
    else
      return s(s'First+1..s'Last); -- Skip the *%"%! front space
    end if;
  end Img;

  function Img(p: PDF_Index_Type) return String is
    s: constant String:= PDF_Index_Type'Image(p);
  begin
    if p < 0 then
      return s;
    else
      return s(s'First+1..s'Last); -- Skip the *%"%! front space
    end if;
  end Img;

  package RIO is new Ada.Text_IO.Float_IO(Real);

  --  Compact real number image, taken from TeXCAD (TeX_Number in tc.adb)
  --
  function Img( x: Real; prec: Positive:= Real'Digits ) return String is
    s: String(1 .. 20 + prec);
    na,nb,np: Natural;
  begin
    RIO.Put(s,x,prec,0);
    na:= s'First;
    nb:= s'Last;
    np:= 0;
    for i in s'Range loop
      case s(i) is
        when '.' => np:= i; exit;  --   Find a decimal point
        when ' ' => na:= i+1;      -- * Trim spaces on left
        when others => null;
      end case;
    end loop;
    if np > 0 then
      while nb > np and then s(nb)='0' loop
        nb:= nb-1;                 -- * Remove extra '0's
      end loop;
      if nb = np then
        nb:= nb-1;                 -- * Remove '.' if it is at the end
      elsif s(na..np-1)= "-0" then
        na:= na+1;
        s(na):= '-';               -- * Reduce "-0.x" to "-.x"
      elsif s(na..np-1)= "0" then
        na:= na+1;                 -- * Reduce "0.x" to ".x"
      end if;
    end if;
    return s(na..nb);
  end Img;

  function "+"(P1,P2: Point) return Point is
  begin
    return ( P1.x+P2.x, P1.y+P2.y );
  end "+";

  function "*"(f: Real; P: Point) return Point is
  begin
    return ( f * P.x, f * P.y );
  end "*";

  function "+"(P: Point; r: Rectangle) return Rectangle is
  begin
    return ( P.x + r.x_min, P.y + r.y_min, r.width, r.height );
  end "+";

  function "*"(f: Real; r: Rectangle) return Rectangle is
  begin
    return ( r.x_min, r.y_min, f * r.width, f * r.height );
  end "*";

  function X_Max(r: Rectangle) return Real is
  begin
    return r.x_min + r.width;
  end;

  function Y_Max(r: Rectangle) return Real is
  begin
    return r.y_min + r.height;
  end;

  type Abs_Rel_Mode is (absolute, relative);

  function Img(p: Point) return String is
  begin
    return Img(p.x) & ' ' & Img(p.y);
  end;

  function Img(box: Rectangle; mode: Abs_Rel_Mode) return String is
  begin
    case mode is
      when absolute =>
        return Img(box.x_min) & ' ' & Img(box.y_min) & ' ' &
               Img(X_Max(box)) & ' ' & Img(Y_Max(box)) & ' ';
      when relative =>
        return Img(box.x_min) & ' ' & Img(box.y_min) & ' ' &
               Img(box.width) & ' ' & Img(box.height) & ' ';
    end case;
  end Img;

  procedure Dispose is new Ada.Unchecked_Deallocation(Offset_table, p_Offset_table);

  procedure New_fixed_index_object(pdf : in out PDF_Out_Stream'Class; idx: PDF_Index_Type) is
    new_table: p_Offset_table;
  begin
    if pdf.object_offset = null then
      pdf.object_offset:= new Offset_table(1..idx);
    elsif pdf.object_offset'Last < idx then
      new_table:= new Offset_table(1..idx*2);
      new_table(1..pdf.object_offset'Last):= pdf.object_offset.all;
      Dispose(pdf.object_offset);
      pdf.object_offset:= new_table;
    end if;
    pdf.object_offset(idx):= Buffer_index(pdf);
    WL(pdf, Img(idx) & " 0 obj");
  end New_fixed_index_object;

  procedure New_object(pdf : in out PDF_Out_Stream'Class) is
  begin
    pdf.objects:= pdf.objects + 1;
    New_fixed_index_object(pdf, pdf.objects);
  end New_object;

  producer: constant String:= "Ada PDF Writer " & version & " - ref: " & reference & " - " & web;

  procedure Write_PDF_header(pdf : in out PDF_Out_Stream'Class) is
  begin
    pdf.is_created:= True;
    pdf.start_index:= Index(pdf);
    case pdf.format is
      when PDF_1_3 =>
        WL(pdf, "%PDF-1.3");
        Byte_buffer'Write(pdf.pdf_stream, (16#25#,16#C2#,16#A5#,16#C2#,16#B1#,16#C3#,16#AB#,10));
    end case;
    WL(pdf, "%  --  Produced by " & producer);
  end Write_PDF_header;

  procedure New_substream(pdf : in out PDF_Out_Stream'Class) is
  begin
    pdf.stream_obj_buf:= Null_Unbounded_String;
  end New_substream;

  procedure Finish_substream(pdf : in out PDF_Out_Stream'Class) is
  begin
    WL(pdf, "  << /Length" & Integer'Image(Length(pdf.stream_obj_buf)) & " >>");
    --  Length could be alternatively stored in next object,
    --  so we wouldn't need to buffer the stream - see 7.3.10, Example 3.
    --  But we prefer the buffered version, which could be compressed in a future version
    --  of this package.
    WL(pdf, "stream");
    WL(pdf, To_String(pdf.stream_obj_buf));
    WL(pdf, "endstream");
  end Finish_substream;

  --  Internal - test page for experimenting PDF constructs (and how Adobe Reader reacts to them)
  --
  procedure Test_Page(pdf: in out PDF_Out_Stream'Class) is
  begin
    WLd(pdf, "10 10 200 200 re S"); -- rectangle, stroke
    WLd(pdf, "  BT");            --  Begin Text object (9.4). Text matrix and text line matrix:= I
    WLd(pdf, "    /Ada_PDF_Std_Font_Helvetica 24 Tf");   --  F1 font, 24 pt size (9.3 Text State Parameters and Operators)
    WLd(pdf, "    0.5 0 0 rg");  --  red, nonstroking colour (Table 74)
    WLd(pdf, "    0.25 G") ;     --  25% gray stroking colour (Table 74)
    WLd(pdf, "    2 Tr");        --  Tr: Set rendering mode as "Fill, then stroke text" (Table 106)
    WLd(pdf, "    20 539 Td");
    WLd(pdf, "    (Hello World !) Tj"); -- Tj: Show a text string (9.4.3 Text-Showing Operators)
    WLd(pdf, "    16 TL");       --  TL: set text leading (distance between lines, 9.3.5)
    WLd(pdf, "    T*");          --  T*: Move to the start of the next line (9.4.2)
    WLd(pdf, "    20 20 200 200 re S"); -- rectangle, stroke (within text region)
    WLd(pdf, "    /Ada_PDF_Std_Font_Helvetica-Oblique 12 Tf");
    WLd(pdf, "    0 Tr");        --  Tr: Set rendering mode as default: "Fill text" (Table 106)
    WLd(pdf, "    0 g");         --  black (default)
    WLd(pdf, "    (Subtitle here.) Tj T*");
    WLd(pdf, "  ET");           --  End Text
    WLd(pdf, "30 30 200 200 re S"); -- rectangle, stroke
    WLd(pdf, "  BT");
    WLd(pdf, "    5 5 Td (Second text chunk here.) Tj T*");
    WLd(pdf, "  ET");
    WLd(pdf, "40 40 240 240 re S"); -- rectangle, stroke
    WLd(pdf, "15 15 Td (Text chunk not within BT/ET.) Tj");
  end Test_Page;

  test_page_mode: constant Boolean:= False;

  --  9.6.2.2  Standard Type 1 Fonts (Standard 14 Fonts)
  function Standard_Font_Name(f: Standard_Font_Type) return String is
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

  function Font_Dictionary_Name(font_name: String) return String is
  begin
    return "/Ada_PDF_Std_Font_" & font_name;  --  Gives the local resource name for any font.
  end Font_Dictionary_Name;

  function Standard_Font_Dictionary_Name(f: Standard_Font_Type) return String is
  begin
    return Font_Dictionary_Name(Standard_Font_Name(f));
  end Standard_Font_Dictionary_Name;

  procedure Font_Dictionary(pdf: in out PDF_Out_Stream'Class) is
  begin
    WL(pdf, "  /Font <<");  --  font dictionary
    for f in Standard_Font_Type loop
      WL(pdf,
        "    " & Standard_Font_Dictionary_Name(f) &
        " << /Type /Font /Subtype /Type1 /BaseFont /" & Standard_Font_Name(f) &
        --  7.9.2.2 Text String Type: "PDFDocEncoding can encode all of
        --  the ISO Latin 1 character set and is documented in Annex D."
        " /Encoding /PDFDocEncoding " &
        " >>"
      );
    end loop;
    WL(pdf, "    >>");
  end Font_Dictionary;

  function Current_Font_Name(pdf: PDF_Out_Stream) return String is
  begin
    if pdf.current_font in Standard_Font_Type then
      return Standard_Font_Name(pdf.current_font);
    else
      return To_String(pdf.ext_font_name);
    end if;
  end Current_Font_Name;

  procedure Insert_PDF_Font_Selection_Code(pdf: in out PDF_Out_Stream) is
  begin
    Insert_PDF_Code(pdf,
      Font_Dictionary_Name(Current_Font_Name(pdf)) &
      ' ' & Img(pdf.font_size) & " Tf " &  --  Tf: 9.3 Text State Parameters and Operators
      Img(pdf.font_size * pdf.line_spacing) & " TL"  --  TL: set text leading (9.3.5)
    );
  end Insert_PDF_Font_Selection_Code;

  procedure Font(pdf: in out PDF_Out_Stream; f: Standard_Font_Type) is
  begin
    pdf.current_font:= f;
    Insert_PDF_Font_Selection_Code(pdf);
  end Font;

  procedure Font_Size(pdf: in out PDF_Out_Stream; size: Real) is
  begin
    pdf.font_size:= size;
    Insert_PDF_Font_Selection_Code(pdf);
  end Font_Size;

  procedure Line_Spacing(pdf: in out PDF_Out_Stream; factor: Real) is
  begin
    pdf.line_spacing:= factor;
    Insert_PDF_Font_Selection_Code(pdf);
  end Line_Spacing;

  procedure Line_Spacing_Pt(pdf: in out PDF_Out_Stream; pt: Real) is
  begin
    pdf.line_spacing:= pt / pdf.font_size;
    Insert_PDF_Font_Selection_Code(pdf);
  end Line_Spacing_Pt;

  procedure Dispose is new Ada.Unchecked_Deallocation(Page_table, p_Page_table);

  procedure New_Page(pdf: in out PDF_Out_Stream) is
    new_table: p_Page_table;
  begin
    if pdf.zone /= nowhere then
      Finish_Page(pdf);
    end if;
    pdf.last_page:= pdf.last_page + 1;
    pdf.current_line:= 1;
    pdf.current_col:= 1;
    PDF_Out.Images.Clear_local_resource_flags(pdf);
    --
    --  Page descriptor object:
    --
    New_object(pdf);
    if pdf.page_idx = null then
      pdf.page_idx:= new Page_table(1..pdf.last_page);
    elsif pdf.page_idx'Last < pdf.last_page then
      new_table:= new Page_table(1..pdf.last_page*2);
      new_table(1..pdf.page_idx'Last):= pdf.page_idx.all;
      Dispose(pdf.page_idx);
      pdf.page_idx:= new_table;
    end if;
    pdf.page_idx(pdf.last_page):= pdf.objects;
    --  Table 30 for options
    WL(pdf, "  <</Type /Page");
    WL(pdf, "    /Parent " & Img(pages_idx) & " 0 R");
    WL(pdf, "    /Contents " & Img(pdf.objects + 1) & " 0 R");   --  Contents stream object is n+1
    WL(pdf, "    /Resources " & Img(pdf.objects + 2) & " 0 R");  --  Resources object is n+2
    WL(pdf, "    /MediaBox [" & Img(pdf.page_box, absolute) & ']');
    WL(pdf, "  >>");
    WL(pdf, "endobj");
    --  Page contents object:
    --
    New_object(pdf);
    New_substream(pdf);
    if test_page_mode then
      Test_Page(pdf);
    else
      pdf.zone:= in_page;
      WLd(pdf, "  BT");            --  Begin Text object (9.4)
      Insert_PDF_Font_Selection_Code(pdf);
      pdf.zone:= in_header;
      Page_Header(PDF_Out_Stream'Class(pdf));
      -- ^ PDF_Out_Stream'Class: make the call to Page_Header dispatching
    end if;
    pdf.zone:= in_page;
    Text_XY(pdf, pdf.page_margins.left, Y_Max(pdf.page_box) - pdf.page_margins.top);
  end New_Page;

  procedure Finish_Page(pdf: in out PDF_Out_Stream) is

    appended_object_idx: PDF_Index_Type;

    procedure Image_Item(dn: in out Dir_node) is
      img_obj: PDF_Index_Type;
    begin
      if dn.local_resource then
        if dn.pdf_object_index = 0 then
          img_obj:= appended_object_idx;
          appended_object_idx:= appended_object_idx + 1;
        else
          img_obj:= dn.pdf_object_index;  --  image has been loaded for a previous page
        end if;
        WL(pdf, Image_name(dn.image_index) & ' ' & Img(img_obj) & " 0 R");
      end if;
    end Image_Item;

    procedure Image_List is new PDF_Out.Images.Traverse_private(Image_Item);

  begin
    if pdf.zone = nowhere then
      return; -- We are already "between pages"
    end if;
    if test_page_mode then
      null;  --  Nothing to do anymore with test page
    else
      pdf.zone:= in_footer;
      Page_Footer(PDF_Out_Stream'Class(pdf));
      -- ^ PDF_Out_Stream'Class: make the call to Page_Header dispatching
      Wd(pdf,  "  ET");            --  End Text
    end if;
    pdf.zone:= nowhere;
    Finish_substream(pdf);
    WL(pdf, "endobj");  --  end of Contents
    New_object(pdf);    --  Resources Dictionary (7.8.3)
    WL(pdf, "<<");
    Font_Dictionary(pdf);
    appended_object_idx:= pdf.objects + 1;  --  Images contents to be appended after this object
    WL(pdf, "  /XObject <<");
    Image_List(pdf);
    WL(pdf, "  >>");
    WL(pdf, ">>");
    WL(pdf, "endobj");  --  end of Resources
    PDF_Out.Images.Insert_unloaded_local_images(pdf);
  end Finish_Page;

  procedure Put(pdf  : in out PDF_Out_Stream;
                num  : in Real;
                fore : in Ada.Text_IO.Field := Real_IO.Default_Fore;
                aft  : in Ada.Text_IO.Field := Real_IO.Default_Aft;
                exp  : in Ada.Text_IO.Field := Real_IO.Default_Exp
            )
  is
  begin
    if exp = 0 then
      declare
        s: String(1 .. fore + 1 + aft);  --  "  123.45"
      begin
        Real_IO.Put(s, num, aft, exp);
        Put(pdf, s);
      end;
    else
      declare
        s: String(1 .. fore + 1 + aft + 1 + exp);  --  "  1.234E-01"
      begin
        Real_IO.Put(s, num, aft, exp);
        Put(pdf, s);
      end;
    end if;
  end Put;

  procedure Put(pdf   : in out PDF_Out_Stream;
                num   : in Integer;
                width : in Ada.Text_IO.Field := 0; -- ignored
                base  : in Ada.Text_IO.Number_Base := 10
            )
  is
  begin
    if base = 10 then
      Put(pdf, Img(num));
    else
      declare
        use Ada.Strings.Fixed;
        s: String(1..50 + 0*width);
        -- "0*width" is just to skip a warning about width being unused
        package IIO is new Ada.Text_IO.Integer_IO(Integer);
      begin
        IIO.Put(s, num, Base => base);
        Put(pdf, Trim(s, Ada.Strings.Left));
      end;
    end if;
  end Put;

  procedure Put(pdf: in out PDF_Out_Stream; str : String) is
  begin
    if test_page_mode then
      null;  --  Nothing to do (test page instead)
    else
      WLd(pdf, "    (" & str & ") Tj");
    end if;
  end Put;

  procedure Put(pdf: in out PDF_Out_Stream; str : Unbounded_String) is
  begin
    Put(pdf, To_String(str));
  end Put;

  procedure Put_Line(pdf  : in out PDF_Out_Stream;
                     num  : in Real;
                     fore : in Ada.Text_IO.Field := Real_IO.Default_Fore;
                     aft  : in Ada.Text_IO.Field := Real_IO.Default_Aft;
                     exp  : in Ada.Text_IO.Field := Real_IO.Default_Exp
            ) is
  begin
    Put(pdf, num, fore, aft, exp);
    New_Line(pdf);
  end Put_Line;

  procedure Put_Line(pdf   : in out PDF_Out_Stream;
                     num   : in Integer;
                     width : in Ada.Text_IO.Field := 0; -- ignored
                     base  : in Ada.Text_IO.Number_Base := 10
            )
  is
  begin
    Put(pdf, num, width, base);
    New_Line(pdf);
  end Put_Line;

  procedure Put_Line(pdf: in out PDF_Out_Stream; str : String) is
  begin
    Put(pdf, str);
    New_Line(pdf);
  end Put_Line;

  procedure Put_Line(pdf: in out PDF_Out_Stream; str : Unbounded_String) is
  begin
    Put_Line(pdf, To_String(str));
  end Put_Line;

  procedure New_Line(pdf: in out PDF_Out_Stream; Spacing : Positive := 1) is
  begin
    pdf.current_line:= pdf.current_line + 1;
    pdf.current_col:= 1;
    if test_page_mode then
      null;  --  Nothing to do (test page instead)
    else
      for i in 1..Spacing loop
        WLd(pdf, "    T*");
      end loop;
    end if;
  end New_Line;

  procedure Text_XY(pdf: in out PDF_Out_Stream; x,y: Real) is
  begin
    --  This is just for resetting the text matrices (hence, position and orientation)
    --
    WLd(pdf, "  ET");            --  End Text
    WLd(pdf, "  BT");            --  Begin Text object (9.4.1, Table 107)
    Insert_PDF_Code(pdf, Img(x) & ' ' & Img(y) & " Td");  --  Td: 9.4.2 Text-Positioning Operators
    pdf.current_line:= 1;
    pdf.current_col:= 1;
  end Text_XY;

  procedure Put_XY(pdf: in out PDF_Out_Stream; x,y: Real; str : String) is
  begin
    Text_XY(pdf, x,y);
    Put(pdf, str);
  end Put_XY;

  function Col(pdf: in PDF_Out_Stream) return Positive is
  begin
    return pdf.current_col;
  end Col;

  function Line(pdf: in PDF_Out_Stream) return Positive is
  begin
    return pdf.current_line;
  end Line;

  function Page(pdf: in PDF_Out_Stream) return Natural is
  begin
    return Natural(pdf.last_page);  --  Issue if Integer is 16-bit and last_page > 2**15-1
  end Page;

  procedure Color(pdf: in out PDF_Out_Stream; c: Color_Type) is
  begin
    Insert_PDF_Code(pdf, Img(c.red) & ' ' & Img(c.green) & ' ' & Img(c.blue) & " rg");
    --  rg = nonstroking colour (Table 74)
  end Color;

  procedure Stroking_Color(pdf: in out PDF_Out_Stream; c: Color_Type) is
  begin
    Insert_PDF_Code(pdf, Img(c.red) & ' ' & Img(c.green) & ' ' & Img(c.blue) & " RG");
    --  RG = nonstroking colour (Table 74)
  end Stroking_Color;

  procedure Text_Rendering_Mode(pdf: in out PDF_Out_Stream; r: Rendering_Mode) is
  begin
    Insert_PDF_Code(pdf, Img(Integer(Rendering_Mode'Pos(r))) & " Tr");
    --  Tr = Set rendering mode (Table 106)
  end;

  function Image_name(i: Positive) return String is
  begin
    return "/Ada_PDF_Img" & Img(i);
  end;

  procedure Image(pdf: in out PDF_Out_Stream; file_name: String; target: Rectangle) is
    image_index: Positive;  --  Index in the list of images
  begin
    if pdf.zone = nowhere then
      New_Page(pdf);
    end if;
    PDF_Out.Images.Image_ref(pdf, file_name, image_index);
    Insert_PDF_Code(pdf, "q " &
      Img(target.width) & " 0 0 " & Img(target.height) &
      ' ' & Img(target.x_min) & ' ' & Img(target.y_min) & " cm " &  --  cm: Table 57
      Image_name(image_index) & " Do Q"
    );
  end;

  function Get_pixel_dimensions(image_file_name: String) return Rectangle is
  begin
    return PDF_Out.Images.Get_pixel_dimensions(image_file_name);
  end Get_pixel_dimensions;

  --  Vector graphics

  procedure Line_Width(pdf: in out PDF_Out_Stream; width: Real) is
  begin
    Insert_PDF_Code(pdf, Img(width) & " w");
  end;

  procedure Single_Line(pdf: in out PDF_Out_Stream; from, to: Point) is
  begin
    Insert_PDF_Code(pdf,
      Img(from.x) & ' ' & Img(from.y) & " m " &
      Img(to.x) & ' ' & Img(to.y) & " l s"
    );
  end;

  --    Table 59 - Path Construction Operators (8.5.2)
  --    Table 60 - Path-Painting Operators (8.5.3.1)

  inside_path_rule_char: constant array(Inside_path_rule) of Character:= (
    nonzero_winding_number => ' ',
    even_odd               => '*'
  );

  path_drawing_operator: constant array(Path_Rendering_Mode) of Character:= (
    fill             => 'F',
    stroke           => 'S',
    fill_then_stroke => 'B'
  );

  procedure Draw(pdf: in out PDF_Out_Stream; what: Rectangle; rendering: Path_Rendering_Mode) is
  begin
    Insert_PDF_Code(pdf, Img(what, relative) & " re " & path_drawing_operator(rendering));
  end;

  procedure Move(pdf: in out PDF_Out_Stream; to: Point) is
  begin
    Insert_PDF_Code(pdf, Img(to) & " m");
  end;

  procedure Line(pdf: in out PDF_Out_Stream; to: Point) is
  begin
    Insert_PDF_Code(pdf, Img(to) & " l");
  end;

  procedure Cubic_Bezier(pdf: in out PDF_Out_Stream; control_1, control_2: Point; to: Point) is
  begin
    Insert_PDF_Code(pdf, Img(control_1) & ' ' & Img(control_2) & ' ' & Img(to) & " c");
  end;

  procedure Finish_Path(
    pdf        : in out PDF_Out_Stream;
    close_path :        Boolean;
    rendering  :        Path_Rendering_Mode;  --  fill, stroke, or both
    rule       :        Inside_path_rule
  )
  is
    cmd: String:= path_drawing_operator(rendering) & inside_path_rule_char(rule);
  begin
    if close_path then
      cmd:= To_Lower(cmd);
    end if;
    --  Insert the s, S, f, f*, b, b*, B, B* of Table 60 - Path-Painting Operators (8.5.3.1)
    if cmd = "s*" or cmd = "S*" or cmd = "F " or cmd = "F*" then
      Insert_PDF_Code(pdf, "n");  --  End the path object without filling or stroking it.
    else
      Insert_PDF_Code(pdf, cmd);
    end if;
  end Finish_Path;

  procedure Insert_PDF_Code(pdf: in out PDF_Out_Stream; code: String) is
  begin
    WLd(pdf, "    " & code);  --  Indentation is just cosmetic...
  end;

  --  Table 317 - Entries in the document information dictionary (14.3.3)

  procedure Title(pdf: in out PDF_Out_Stream; s: String) is
  begin
    pdf.doc_title:= To_Unbounded_String(s);
  end;

  procedure Author(pdf: in out PDF_Out_Stream; s: String) is
  begin
    pdf.doc_author:= To_Unbounded_String(s);
  end;

  procedure Subject(pdf: in out PDF_Out_Stream; s: String) is
  begin
    pdf.doc_subject:= To_Unbounded_String(s);
  end;

  procedure Keywords(pdf: in out PDF_Out_Stream; s: String) is
  begin
    pdf.doc_keywords:= To_Unbounded_String(s);
  end;

  procedure Creator_Application(pdf: in out PDF_Out_Stream; s: String) is
  begin
    pdf.doc_creator:= To_Unbounded_String(s);
  end;

  procedure Page_Header(pdf : in out PDF_Out_Stream) is
  begin
    null;  --  Default header is empty.
  end;

  procedure Page_Footer(pdf : in out PDF_Out_Stream) is
  begin
    null;  --  Default footer is empty.
  end;

  procedure Left_Margin(pdf : out PDF_Out_Stream; pts: Real) is
  begin
    pdf.page_margins.left:= pts;
  end Left_Margin;

  function Left_Margin(pdf : PDF_Out_Stream) return Real is
  begin
    return pdf.page_margins.left;
  end Left_Margin;

  procedure Right_Margin(pdf : out PDF_Out_Stream; pts: Real) is
  begin
    pdf.page_margins.right:= pts;
  end Right_Margin;

  function Right_Margin(pdf : PDF_Out_Stream) return Real is
  begin
    return pdf.page_margins.right;
  end Right_Margin;

  procedure Top_Margin(pdf : out PDF_Out_Stream; pts: Real) is
  begin
    pdf.page_margins.top:= pts;
  end Top_Margin;

  function Top_Margin(pdf : PDF_Out_Stream) return Real is
  begin
    return pdf.page_margins.top;
  end Top_Margin;

  procedure Bottom_Margin(pdf : out PDF_Out_Stream; pts: Real) is
  begin
    pdf.page_margins.bottom:= pts;
  end Bottom_Margin;

  function Bottom_Margin(pdf : PDF_Out_Stream) return Real is
  begin
    return pdf.page_margins.bottom;
  end Bottom_Margin;

  procedure Margins(pdf : out PDF_Out_Stream; new_margins: Margins_Type) is
  begin
    pdf.page_margins:= new_margins;
  end Margins;

  function Margins(pdf : PDF_Out_Stream) return Margins_Type is
  begin
    return pdf.page_margins;
  end Margins;

  procedure Page_Setup(pdf : in out PDF_Out_Stream; layout: Rectangle) is
    mb_x_min, mb_y_min, mb_x_max, mb_y_max: Real;
  begin
    pdf.page_box:= layout;
    mb_x_min:= Real'Min(pdf.maximum_box.x_min, layout.x_min);
    mb_y_min:= Real'Min(pdf.maximum_box.y_min, layout.y_min);
    mb_x_max:= Real'Max(X_Max(pdf.maximum_box), X_Max(layout));
    mb_y_max:= Real'Max(Y_Max(pdf.maximum_box), Y_Max(layout));
    pdf.maximum_box:=
      ( x_min  => mb_x_min,
        y_min  => mb_y_min,
        width  => mb_x_max - mb_x_min,
        height => mb_y_max - mb_y_min
      );
  end Page_Setup;

  function Layout(pdf : PDF_Out_Stream) return Rectangle is
  begin
    return pdf.page_box;
  end Layout;

  procedure Reset(
    pdf        : in out PDF_Out_Stream'Class;
    PDF_format :        PDF_type:= Default_PDF_type
  )
  is
    dummy_pdf_with_defaults: PDF_Out_Pre_Root_Type;
  begin
    -- Check if we are trying to re-use a half-finished object (ouch!):
    if pdf.is_created and not pdf.is_closed then
      raise PDF_stream_not_closed;
    end if;
    -- We will reset everything with defaults, except this:
    dummy_pdf_with_defaults.format:= PDF_format;
    -- Now we reset pdf:
    PDF_Out_Pre_Root_Type(pdf):= dummy_pdf_with_defaults;
  end Reset;

  procedure Finish(pdf : in out PDF_Out_Stream) is

    info_idx, cat_idx: PDF_Index_Type;

    procedure Info is
    begin
      New_object(pdf);
      info_idx:= pdf.objects;
      WL(pdf, "  << /Producer (" & producer & ')');
      WL(pdf, "     /Title (" & To_String(pdf.doc_title) & ')');
      WL(pdf, "     /Author (" & To_String(pdf.doc_author) & ')');
      WL(pdf, "     /Subject (" & To_String(pdf.doc_subject) & ')');
      WL(pdf, "     /Keywords (" & To_String(pdf.doc_keywords) & ')');
      WL(pdf, "     /Creator (" & To_String(pdf.doc_creator) & ')');
      WL(pdf, "  >>");
      WL(pdf, "endobj");
    end Info;

    procedure Pages_dictionary is
    begin
      New_fixed_index_object(pdf, pages_idx);
      WL(pdf, "  << /Type /Pages");
      W(pdf,  "     /Kids [");
      for p in 1..pdf.last_page loop
        W(pdf, Img(pdf.page_idx(p)) & " 0 R ");
      end loop;
      WL(pdf, "]");
      if pdf.last_page > 0 then
        WL(pdf, "     /Count " & Img(pdf.last_page));
      end if;
      WL(pdf, "     /MediaBox [" & Img(pdf.maximum_box, absolute) & ']'
      );
      --  7.7.3.3 Page Objects - MediaBox
      --  Boundaries of the physical medium on which the page shall be displayed or printed
      --  7.7.3.4 Inheritance of Page Attributes
      --  Global page size, lower-left to upper-right, measured in points
      --  Bounding box of all pages
      WL(pdf, "  >>");
      WL(pdf, "endobj");
    end Pages_dictionary;

    procedure Catalog_dictionary is
    begin
      New_object(pdf);
      cat_idx:= pdf.objects;
      WL(pdf, "  << /Type /Catalog");
      WL(pdf, "     /Pages " & Img(pages_idx) & " 0 R");
      if pdf.last_page > 0 then
        WL(pdf, "     /OpenAction [" & Img(pdf.page_idx(1)) & " 0 R /Fit]");
        --  ^ Open on page 1, fit the entire page within the window (Table 151)
      end if;
      WL(pdf, "  >>");
      WL(pdf, "endobj");
    end Catalog_dictionary;

    procedure Trailer is
    begin
      WL(pdf, "trailer");
      WL(pdf, "  << /Root " & Img(cat_idx) & " 0 R");
      WL(pdf, "     /Size " & Img(pdf.objects+1));
      WL(pdf, "     /Info " & Img(info_idx) & " 0 R");
      WL(pdf, "  >>");
    end Trailer;

    xref_offset: Ada.Streams.Stream_IO.Count;

    procedure XRef is
      s10: String(1..10);
    begin
      xref_offset:= Buffer_index(pdf);
      WL(pdf, "xref");
      WL(pdf, "0 " & Img(pdf.objects+1));
      WL(pdf, "0000000000 65535 f ");
      for i in 1..pdf.objects loop
        CIO.Put(s10, pdf.object_offset(i));
        for n in s10'Range loop
          if s10(n)=' ' then
            s10(n):= '0';
          end if;
        end loop;
        WL(pdf, s10 & " 00000 n "); -- <- trailing space needed!
      end loop;
    end XRef;

  begin
    if pdf.last_page = 0 then
      -- No page ? Then make quickly a blank page
      New_Page(pdf);
    end if;
    Finish_Page(pdf);
    Info;
    Pages_dictionary;
    Catalog_dictionary;
    XRef;
    Trailer;
    WL(pdf, "startxref"); -- offset of xref
    WL(pdf, Img(Integer(xref_offset)));
    WL(pdf, "%%EOF");
    Dispose(pdf.page_idx);
    Dispose(pdf.object_offset);
    PDF_Out.Images.Clear_image_directory(pdf);
    pdf.is_closed:= True;
  end Finish;

  ----------------------
  -- Output to a file --
  ----------------------

  procedure Create(
    pdf        : in out PDF_Out_File;
    file_name  :        String;
    PDF_format :        PDF_type:= Default_PDF_type
  )
  is
  begin
    Reset(pdf, PDF_format);
    pdf.pdf_file:= new Ada.Streams.Stream_IO.File_Type;
    Create(pdf.pdf_file.all, Out_File, file_name);
    pdf.file_name:= To_Unbounded_String(file_name);
    pdf.pdf_stream:= PDF_Raw_Stream_Class(Stream(pdf.pdf_file.all));
    Write_PDF_header(pdf);
  end Create;

  procedure Close(pdf : in out PDF_Out_File) is
    procedure Dispose is new
      Ada.Unchecked_Deallocation(Ada.Streams.Stream_IO.File_Type, PDF_file_acc);
  begin
    Finish(PDF_Out_Stream(pdf));
    if pdf.file_name /= "nul" then  --  Test needed for OA 7.2.2 (Close raises Use_Error)
      Close(pdf.pdf_file.all);
    end if;
    Dispose(pdf.pdf_file);
  end Close;

  -- Set the index on the file
  procedure Set_Index (pdf: in out PDF_Out_File;
                       To: Ada.Streams.Stream_IO.Positive_Count)
  is
  begin
    Ada.Streams.Stream_IO.Set_Index(pdf.pdf_file.all, To);
  end Set_Index;

  -- Return the index of the file
  function Index (pdf: PDF_Out_File) return Ada.Streams.Stream_IO.Count
  is
  begin
    return Ada.Streams.Stream_IO.Index(pdf.pdf_file.all);
  end Index;

  function Is_Open(pdf : in PDF_Out_File) return Boolean is
  begin
    if pdf.pdf_file = null then
      return False;
    end if;
    return Ada.Streams.Stream_IO.Is_Open(pdf.pdf_file.all);
  end Is_Open;

  ------------------------
  -- Output to a string --
  ------------------------
  -- Code reused from Zip_Streams

  procedure Read
    (Stream : in out Unbounded_Stream;
     Item   : out Stream_Element_Array;     Last   : out Stream_Element_Offset) is
  begin
    -- Item is read from the stream. If (and only if) the stream is
    -- exhausted, Last will be < Item'Last. In that case, T'Read will
    -- raise an End_Error exception.
    --
    -- Cf: RM 13.13.1(8), RM 13.13.1(11), RM 13.13.2(37) and
    -- explanations by Tucker Taft
    --
    Last:= Item'First - 1;
    -- if Item is empty, the following loop is skipped; if Stream.Loc
    -- is already indexing out of Stream.Unb, that value is also appropriate
    for i in Item'Range loop
      Item(i) := Character'Pos (Element(Stream.Unb, Stream.Loc));
      Stream.Loc := Stream.Loc + 1;
      Last := i;
    end loop;
  exception
    when Ada.Strings.Index_Error =>
      null; -- what could be read has been read; T'Read will raise End_Error
  end Read;

  procedure Write
    (Stream : in out Unbounded_Stream;
     Item   : Stream_Element_Array) is
  begin
    for I in Item'Range loop
      if Length(Stream.Unb) < Stream.Loc then
        Append(Stream.Unb, Character'Val(Item(I)));
      else
        Replace_Element(Stream.Unb, Stream.Loc, Character'Val(Item(I)));
      end if;
      Stream.Loc := Stream.Loc + 1;
    end loop;
  end Write;

  procedure Set_Index (S : access Unbounded_Stream; To : Positive) is
  begin
    if Length(S.Unb) < To then
      for I in Length(S.Unb) .. To loop
        Append(S.Unb, ASCII.NUL);
      end loop;
    end if;
    S.Loc := To;
  end Set_Index;

  function Index (S : access Unbounded_Stream) return Integer is
  begin
    return S.Loc;
  end Index;

  --- ***

  procedure Create(
    pdf        : in out PDF_Out_String;
    PDF_format :        PDF_type:= Default_PDF_type
  )
  is
  begin
    Reset(pdf, PDF_format);
    pdf.pdf_memory:= new Unbounded_Stream;
    pdf.pdf_memory.Unb:= Null_Unbounded_String;
    pdf.pdf_memory.Loc:= 1;
    pdf.pdf_stream:= PDF_Raw_Stream_Class(pdf.pdf_memory);
    Write_PDF_header(pdf);
  end Create;

  procedure Close(pdf : in out PDF_Out_String) is
  begin
    Finish(PDF_Out_Stream(pdf));
  end Close;

  function Contents(pdf: PDF_Out_String) return String is
  begin
    if not pdf.is_closed then
      raise PDF_stream_not_closed;
    end if;
    return To_String(pdf.pdf_memory.Unb);
  end Contents;

  -- Set the index on the PDF string stream
  procedure Set_Index (pdf: in out PDF_Out_String;
                       To: Ada.Streams.Stream_IO.Positive_Count)
  is
  begin
    Set_Index(pdf.pdf_memory, Integer(To));
  end Set_Index;

  -- Return the index of the PDF string stream
  function Index (pdf: PDF_Out_String) return Ada.Streams.Stream_IO.Count
  is
  begin
    return Ada.Streams.Stream_IO.Count(Index(pdf.pdf_memory));
  end Index;

end PDF_Out;
