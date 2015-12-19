with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;
-- with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;

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
    New_Page(pdf);
  end Write_PDF_header;

  procedure Test_Page(pdf: in out PDF_Out_Stream'Class) is
  begin
    New_object(pdf);
    WL(pdf, "  << /Length 60 >>");
    --  A stream, 60 bytes in length (after "stream" row, before "endstream" row)
    WL(pdf, "stream");
    WL(pdf, "  BT");            --  Begin Text object
    WL(pdf, "    /F1 24 Tf");   --  Use F1 font at 24 point size
    WL(pdf, "    20 " & Img(pdf.page_max_y - 56) & " Td");  -- Text position
    WL(pdf, "    (Hello World !) Tj"); -- show
    WL(pdf, "  ET");            --  End Text
    WL(pdf, "endstream");
    WL(pdf, "endobj");
  end Test_Page;

  procedure Page_finish(pdf: in out PDF_Out_Stream'Class) is
  begin
    if pdf.zone = nowhere then
      return; -- We are already "between pages"
    end if;
    Test_Page(pdf); -- !!
    pdf.zone:= in_footer;
    Page_Footer(pdf);
    pdf.zone:= nowhere;
  end Page_finish;

  procedure Test_Font(pdf: in out PDF_Out_Stream'Class) is
  begin
    WL(pdf, "<< /Font");
    WL(pdf, "  << /F1");
    WL(pdf, "    << /Type /Font");
    WL(pdf, "       /Subtype /Type1");
    WL(pdf, "       /BaseFont /Times-Roman");
    WL(pdf, "    >>");
    WL(pdf, "  >>");
    WL(pdf, ">>");
  end;

  procedure New_Page(pdf: in out PDF_Out_Stream'Class) is
  begin
    if pdf.zone /= nowhere then
      Page_finish(pdf);
    end if;
    pdf.last_page:= pdf.last_page + 1;
    New_object(pdf);
    pdf.page_idx(pdf.last_page):= pdf.objects;
    pdf.page_min_x:= 0; -- !! hardcoded
    pdf.page_min_y:= 0; -- !! hardcoded
    pdf.page_max_x:= 420; -- !! hardcoded
    pdf.page_max_y:= 540; -- !! hardcoded
    WL(pdf, "  <</Type /Page");
    WL(pdf, "    /Parent " & Img(pages_idx) & " 0 R");
    WL(pdf, "    /Resources");
    Test_Font(pdf); -- !!
    WL(pdf, "    /Contents " & Img(pdf.objects+1) & " 0 R");
    --  ^ The Contents stream object is coming just after this Page object
    WL(pdf, "  >>");
    WL(pdf, "endobj");
    pdf.zone:= in_header;
    Page_Header(pdf);
    pdf.zone:= in_page;
  end New_Page;

  procedure Put(pdf: in out PDF_Out_Stream; num : Long_Float) is
  begin
    null; -- !!
  end Put;

  procedure Put(pdf    : in out PDF_Out_Stream;
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
        -- 0*width is just to skip a warning of width being unused
        package IIO is new Ada.Text_IO.Integer_IO(Integer);
      begin
        IIO.Put(s, num, Base => base);
        Put(pdf, Trim(s, Ada.Strings.Left));
      end;
    end if;
  end Put;

  procedure Put(pdf: in out PDF_Out_Stream; str : String) is
  begin
    null; -- !!
  end Put;

  procedure Put(pdf: in out PDF_Out_Stream; str : Unbounded_String) is
  begin
    Put(pdf, To_String(str));
  end Put;

  procedure Put(pdf: in out PDF_Out_Stream; date: Time) is
  begin
    null; -- !!
  end Put;

  procedure Put_Line(pdf: in out PDF_Out_Stream; num : Long_Float) is
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

  procedure New_Line(pdf: in out PDF_Out_Stream'Class; Spacing : Positive := 1) is
  begin
    null; -- !!
  end New_Line;

  function Col(pdf: in PDF_Out_Stream) return Positive is
  pragma Unreferenced (pdf);
  begin
    return 1; -- !!
  end Col;

  function Line(pdf: in PDF_Out_Stream) return Positive is
  pragma Unreferenced (pdf);
  begin
    return 1; -- !!
  end Line;

  function Page(pdf: in PDF_Out_Stream) return Positive is
  begin
    return pdf.last_page;
  end Page;

  procedure Page_Header(pdf : PDF_Out_Stream) is
  begin
    null;
  end;

  procedure Page_Footer(pdf : PDF_Out_Stream) is
  begin
    null;
  end;

  procedure Left_Margin(pdf : PDF_Out_Stream; inches: Long_Float) is
  begin
    null; -- !!
  end Left_Margin;

  procedure Right_Margin(pdf : PDF_Out_Stream; inches: Long_Float) is
  begin
    null; -- !!
  end Right_Margin;

  procedure Top_Margin(pdf : PDF_Out_Stream; inches: Long_Float) is
  begin
    null; -- !!
  end Top_Margin;

  procedure Bottom_Margin(pdf : PDF_Out_Stream; inches: Long_Float) is
  begin
    null; -- !!
  end Bottom_Margin;

  procedure Margins(pdf : PDF_Out_Stream; left, right, top, bottom: Long_Float) is
  begin
    null; -- !!
  end Margins;

  procedure Reset(
    pdf           : in out PDF_Out_Stream'Class;
    PDF_format :        PDF_type:= Default_PDF_type
  )
  is
    dummy_pdf_with_defaults: PDF_Out_Pre_Root_Type;
  begin
    -- Check if we are trying to re-use a half-finished object (ouch!):
    if pdf.is_created and not pdf.is_closed then
      raise PDF_stream_not_closed;
    end if;
    -- We will reset evything with defaults, except this:
    dummy_pdf_with_defaults.format:= PDF_format;
    -- Now we reset pdf:
    PDF_Out_Pre_Root_Type(pdf):= dummy_pdf_with_defaults;
  end Reset;

  procedure Finish(pdf : in out PDF_Out_Stream'Class) is

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
      WL(pdf, "    /MediaBox [" &
        Img(pdf.page_min_x) & ' ' &
        Img(pdf.page_min_y) & ' ' &
        Img(pdf.page_max_x) & ' ' &
        Img(pdf.page_max_y) & ']'
      ); -- !! probably bounding box of all pages here
      -- Global page size, lower-left to upper-right, measured in points
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
    pdf           : in out PDF_Out_File;
    file_name    :        String;
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
    Finish(pdf);
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
    pdf           : in out PDF_Out_String;
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
    Finish(pdf);
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
