--  All technical references are to PDF 1.7 format, ISO 32000-1:2008 standard
--  http://www.adobe.com/devnet/pdf/pdf_reference.html
--

with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;

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
  pragma Unreferenced (Block_Write);

  procedure W(pdf : in out PDF_Out_Stream'Class; s: String) is
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
  begin
    Append(pdf.stream_obj_buf, s);
  end;

  procedure WLd(pdf : in out PDF_Out_Stream'Class; s: String) is
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
  end;

  package RIO is new Ada.Text_IO.Float_IO(Real);

  --  Compact real number image, taken from TeXCAD (TeX_Number in tc.adb)
  --
  function Img( x: Real; prec: Positive:= Real'Digits ) return String is
    s: String(1..30);
    na,nb,np:Natural;
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

  function Img(box: Rectangle) return String is
  begin
    return
      Img(box.x_min) & ' ' &
      Img(box.y_min) & ' ' &
      Img(box.x_max) & ' ' &
      Img(box.y_max) & ' ';
  end Img;

  procedure New_fixed_object(pdf : in out PDF_Out_Stream'Class; idx: Positive) is
  begin
    pdf.object_offset(idx):= Buffer_index(pdf);
    WL(pdf, Img(idx) & " 0 obj");
  end New_fixed_object;

  procedure New_object(pdf : in out PDF_Out_Stream'Class) is
  begin
    pdf.objects:= pdf.objects + 1;
    New_fixed_object(pdf, pdf.objects);
  end New_object;

  procedure Write_PDF_header(pdf : in out PDF_Out_Stream'Class) is
  begin
    pdf.is_created:= True;
    pdf.start_index:= Index(pdf);
    case pdf.format is
      when PDF_1_3 =>
        WL(pdf, "%PDF-1.3");
        Byte_buffer'Write(pdf.pdf_stream,
          (16#25#, 16#C2#, 16#A5#, 16#C2#, 16#B1#, 16#C3#, 16#AB#)
        );
        WL(pdf, "");
    end case;
  end Write_PDF_header;

  procedure New_stream(pdf : in out PDF_Out_Stream'Class) is
  begin
    pdf.stream_obj_buf:= Null_Unbounded_String;
  end New_stream;

  procedure Finish_stream(pdf : in out PDF_Out_Stream'Class) is
  begin
    WL(pdf, "  << /Length" & Integer'Image(Length(pdf.stream_obj_buf)) & " >>");
    --  Length could be alternatively stored in next object,
    --  so we wouldn't need to buffer the stream - see 7.3.10, Example 3.
    --  But we prefer the buffered version, which could be compressed in a future version
    --  of this package.
    WL(pdf, "stream");
    WL(pdf, To_String(pdf.stream_obj_buf));
    WL(pdf, "endstream");
  end Finish_stream;

  --  Internal - test page for experimenting PDF constructs (and how Adobe Reader reacts to them)
  --
  procedure Test_Page(pdf: in out PDF_Out_Stream'Class) is
  begin
    WLd(pdf, "10 10 210 210 re S"); -- rectangle, stroke
    WLd(pdf, "  BT");            --  Begin Text object (9.4). Text matrix and text line matrix:= I
    WLd(pdf, "    /F1 24 Tf");   --  F1 font, 24 pt size (9.3 Text State Parameters and Operators)
    WLd(pdf, "    0.5 0 0 rg");  --  red, nonstroking colour (Table 74)
    WLd(pdf, "    0.25 G") ;     --  25% gray stroking colour (Table 74)
    WLd(pdf, "    2 Tr");        --  Tr: Set rendering mode as "Fill, then stroke text" (Table 106)
    WLd(pdf, "    20 " & Img(pdf.page_box.y_max - 56.0) & " Td");  -- 9.4.2 Text-Positioning Operators
    WLd(pdf, "    (Hello World !) Tj"); -- Tj: Show a text string (9.4.3 Text-Showing Operators)
    WLd(pdf, "    16 TL");       --  TL: set text leading (distance between lines, 9.3.5)
    WLd(pdf, "    T*");          --  T*: Move to the start of the next line (9.4.2)
    WLd(pdf, "    20 20 220 220 re S"); -- rectangle, stroke (within text region)
    WLd(pdf, "    /F2 12 Tf");
    WLd(pdf, "    0 Tr");        --  Tr: Set rendering mode as default: "Fill text" (Table 106)
    WLd(pdf, "    0 g");         --  black (default)
    WLd(pdf, "    (Subtitle here.) Tj T*");
    WLd(pdf, "  ET");           --  End Text
    WLd(pdf, "30 30 230 230 re S"); -- rectangle, stroke
    WLd(pdf, "  BT");
    WLd(pdf, "    5 5 Td (Second text chunk here.) Tj T*");
    WLd(pdf, "  ET");
    WLd(pdf, "40 40 240 240 re S"); -- rectangle, stroke
    WLd(pdf, "15 15 Td (Text chunk not within BT/ET.) Tj");
  end Test_Page;

  test_page_mode: constant Boolean:= False;

  procedure Test_Font(pdf: in out PDF_Out_Stream'Class) is
  begin
    WL(pdf, "<< /Font");
    WL(pdf, "  << /F1 << /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>");
    WL(pdf, "     /F2 << /Type /Font /Subtype /Type1 /BaseFont /Helvetica-Oblique >>");
    WL(pdf, "     /F3 << /Type /Font /Subtype /Type1 /BaseFont /Times-Roman >>");
    WL(pdf, "  >>");
    WL(pdf, ">>");
  end Test_Font;

  --  Internal, called by New_Page and Finish to finish current page
  --
  procedure Page_finish(pdf: in out PDF_Out_Stream);

  procedure New_Page(pdf: in out PDF_Out_Stream) is
  begin
    if pdf.zone /= nowhere then
      Page_finish(pdf);
    end if;
    pdf.last_page:= pdf.last_page + 1;
    pdf.current_line:= 1;
    pdf.current_col:= 1;
    --
    --  Page descriptor object:
    --
    New_object(pdf);
    pdf.page_idx(pdf.last_page):= pdf.objects;
    --  Table 30 for options
    WL(pdf, "  <</Type /Page");
    WL(pdf, "    /Parent " & Img(pages_idx) & " 0 R");
    WL(pdf, "    /Resources");
    Test_Font(pdf); -- !!
    WL(pdf, "    /Contents " & Img(pdf.objects+1) & " 0 R");
    --  ^ The Contents stream object is coming just after this Page object
    WL(pdf, "    /MediaBox [" & Img(pdf.page_box) & ']');
    WL(pdf, "  >>");
    WL(pdf, "endobj");
    --  Page contents object:
    --
    New_object(pdf);
    New_stream(pdf);
    if test_page_mode then
      Test_Page(pdf);
    else
      WLd(pdf, "  BT");            --  Begin Text object (9.4)
      WLd(pdf, "    /F1 12 Tf");   --  F1 font (9.3 Text State Parameters and Operators)
      -- Td: 9.4.2 Text-Positioning Operators
      Text_XY(pdf, pdf.page_margins.left, pdf.page_box.y_max - pdf.page_margins.top);
      WLd(pdf, "    16 TL");       --  TL: set text leading (distance between lines, 9.3.5)
      pdf.zone:= in_header;
      Page_Header(pdf);
    end if;
    pdf.zone:= in_page;
  end New_Page;

  procedure Page_finish(pdf: in out PDF_Out_Stream) is
  begin
    if pdf.zone = nowhere then
      return; -- We are already "between pages"
    end if;
    if test_page_mode then
      null;  --  Nothing to do anymore with test page
    else
      pdf.zone:= in_footer;
      Page_Footer(pdf);
      Wd(pdf,  "  ET");            --  End Text
    end if;
    pdf.zone:= nowhere;
    Finish_stream(pdf);
    WL(pdf, "endobj");
  end Page_finish;

  procedure Put(pdf: in out PDF_Out_Stream; num : Real) is
  begin
    null; -- !!
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
    if pdf.zone = nowhere then
      New_Page(pdf);
    end if;
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

  procedure Put(pdf: in out PDF_Out_Stream; date: Time) is
  begin
    null; -- !!
  end Put;

  procedure Put_Line(pdf: in out PDF_Out_Stream; num : Real) is
  begin
    Put(pdf, num);
    New_Line(pdf);
  end Put_Line;

  procedure Put_Line(pdf: in out PDF_Out_Stream; num : Integer) is
  begin
    Put(pdf, num);
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

  procedure Put_Line(pdf: in out PDF_Out_Stream; date: Time) is
  begin
    Put(pdf, date);
    New_Line(pdf);
  end Put_Line;

  procedure New_Line(pdf: in out PDF_Out_Stream; Spacing : Positive := 1) is
  begin
    if pdf.zone = nowhere then
      New_Page(pdf);
    end if;
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

  procedure Text_XY(pdf: in out PDF_Out_Stream; x,y: Long_Float) is
  begin
    --  This is just for resetting the text matrices (hence, position and orientation)
    --
    WLd(pdf, "  ET");            --  End Text
    WLd(pdf, "  BT");            --  Begin Text object (9.4.1, Table 107)
    WLd(pdf, "    " & Img(x) & ' ' & Img(y) & " Td");  --  Td: 9.4.2 Text-Positioning Operators
  end Text_XY;

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
    return pdf.last_page;
  end Page;

  procedure Page_Header(pdf : in out PDF_Out_Stream) is
  begin
    null;  --  Default header is empty.
    --
    --Test_Text(pdf); -- !! To be removed !!
  end;

  procedure Page_Footer(pdf : in out PDF_Out_Stream) is
  begin
    null;  --  Default footer is empty.
  end;

  procedure Left_Margin(pdf : in out PDF_Out_Stream; pts: Real) is
  begin
    pdf.page_margins.left:= pts;
  end Left_Margin;

  procedure Right_Margin(pdf : in out PDF_Out_Stream; pts: Real) is
  begin
    pdf.page_margins.right:= pts;
  end Right_Margin;

  procedure Top_Margin(pdf : in out PDF_Out_Stream; pts: Real) is
  begin
    pdf.page_margins.top:= pts;
  end Top_Margin;

  procedure Bottom_Margin(pdf : in out PDF_Out_Stream; pts: Real) is
  begin
    pdf.page_margins.bottom:= pts;
  end Bottom_Margin;

  procedure Margins(pdf : in out PDF_Out_Stream; new_margins: Margins_Type) is
  begin
    pdf.page_margins:= new_margins;
  end Margins;

  procedure Page_Setup(pdf : in out PDF_Out_Stream; layout: Rectangle) is
  begin
    pdf.page_box:= layout;
    pdf.maximum_box:=
      ( x_min => Real'Min(pdf.maximum_box.x_min, layout.x_min),
        y_min => Real'Min(pdf.maximum_box.y_min, layout.y_min),
        x_max => Real'Max(pdf.maximum_box.x_max, layout.x_max),
        y_max => Real'Max(pdf.maximum_box.y_max, layout.y_max) );
  end Page_Setup;

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

    info_idx, cat_idx: Positive;

    procedure Info is
    begin
      New_object(pdf);
      info_idx:= pdf.objects;
      WL(pdf, "  <<");
      WL(pdf, "    /Producer (Ada PDF Writer " & version &
              " - ref: " & reference & " - " & web & ")");
      WL(pdf, "  >>");
      WL(pdf, "endobj");
    end Info;

    procedure Pages_dictionary is
    begin
      New_fixed_object(pdf, pages_idx);
      WL(pdf, "  <<");
      WL(pdf, "    /Type /Pages");
      W(pdf,  "    /Kids [");
      for p in 1..pdf.last_page loop
        W(pdf, Img(pdf.page_idx(p)) & " 0 R ");
      end loop;
      WL(pdf, "]");
      if pdf.last_page > 0 then
        WL(pdf, "    /Count " & Img(pdf.last_page));
      end if;
      WL(pdf, "    /MediaBox [" & Img(pdf.maximum_box) & ']'
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
      WL(pdf, "  <<");
      WL(pdf, "    /Type /Catalog");
      WL(pdf, "    /Pages " & Img(pages_idx) & " 0 R");
      if pdf.last_page > 0 then
        WL(pdf, "    /OpenAction [" & Img(pdf.page_idx(1)) & " 0 R /Fit]");
        --  ^ Open on page 1, fit the entire page within the window (Table 151)
      end if;
      WL(pdf, "  >>");
      WL(pdf, "endobj");
    end Catalog_dictionary;

    procedure Trailer is
    begin
      WL(pdf, "trailer");
      WL(pdf, "  <<");
      WL(pdf, "    /Root " & Img(cat_idx) & " 0 R");
      WL(pdf, "    /Size " & Img(pdf.objects+1));
      WL(pdf, "    /Info " & Img(info_idx) & " 0 R");
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
    Page_finish(pdf);
    Info;
    Pages_dictionary;
    Catalog_dictionary;
    XRef;
    Trailer;
    WL(pdf, "startxref"); -- offset of xref
    WL(pdf, Img(Integer(xref_offset)));
    WL(pdf, "%%EOF");
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
    pdf.pdf_stream:= PDF_Raw_Stream_Class(Stream(pdf.pdf_file.all));
    Write_PDF_header(pdf);
  end Create;

  procedure Close(pdf : in out PDF_Out_File) is
    procedure Dispose is new
      Ada.Unchecked_Deallocation(Ada.Streams.Stream_IO.File_Type, PDF_file_acc);
  begin
    Finish(PDF_Out_Stream(pdf));
    Close(pdf.pdf_file.all);
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
     Item   : out Stream_Element_Array;
     Last   : out Stream_Element_Offset) is
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
