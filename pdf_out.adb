with PDF_Out.Fonts,
     PDF_Out.Images;

with GID;

with Ada.Characters.Conversions,
     Ada.Characters.Handling,
     Ada.Strings.Fixed,
     Ada.Unchecked_Deallocation;

with Interfaces;

package body PDF_Out is

  use Ada.Streams.Stream_IO, Ada.Streams;
  use Interfaces;

  package CIO is new Ada.Text_IO.Integer_IO (Ada.Streams.Stream_IO.Count);

  type Byte_Buffer is array (Integer range <>) of Unsigned_8;

  --  Workaround for the severe xxx'Read xxx'Write performance
  --  problems in the GNAT and ObjectAda compilers (as in 2009)
  --  This is possible if and only if Byte = Stream_Element and
  --  arrays types are both packed and aligned the same way.
  --
  subtype Size_test_a is Byte_Buffer (1 .. 19);
  subtype Size_test_b is Ada.Streams.Stream_Element_Array (1 .. 19);
  workaround_possible : constant Boolean :=
    Size_test_a'Size = Size_test_b'Size and
    Size_test_a'Alignment = Size_test_b'Alignment;

  procedure Block_Read
    (file          : in     Ada.Streams.Stream_IO.File_Type;
     buffer        :    out Byte_Buffer;
     actually_read :    out Natural)
  is
    SE_Buffer   : Stream_Element_Array (1 .. buffer'Length);
    for SE_Buffer'Address use buffer'Address;
    pragma Import (Ada, SE_Buffer);
    Last_Read   : Stream_Element_Offset;
  begin
    if workaround_possible then
      Read (Stream (file).all, SE_Buffer, Last_Read);
      actually_read := Natural (Last_Read);
    else
      if End_Of_File (file) then
        actually_read := 0;
      else
        actually_read :=
          Integer'Min (buffer'Length, Integer (Size (file) - Index (file) + 1));
        Byte_Buffer'Read
          (Stream (file),
           buffer (buffer'First .. buffer'First + actually_read - 1));
      end if;
    end if;
  end Block_Read;

  procedure Block_Write
    (stream : in out Ada.Streams.Root_Stream_Type'Class;
     buffer : in     Byte_Buffer)
  is
    pragma Inline (Block_Write);
    SE_Buffer   : Stream_Element_Array (1 .. buffer'Length);
    for SE_Buffer'Address use buffer'Address;
    pragma Import (Ada, SE_Buffer);
  begin
    if workaround_possible then
      Ada.Streams.Write (stream, SE_Buffer);
    else
      Byte_Buffer'Write (stream'Access, buffer);
      --  ^ This was 30x to 70x slower on GNAT 2009
      --    Test in the Zip-Ada project.
    end if;
  end Block_Write;

  --  Copy a whole file into a stream, using a temporary buffer
  procedure Copy_File
    (file_name   :        String;
     into        : in out Ada.Streams.Root_Stream_Type'Class;
     buffer_size :        Positive := 1024 * 1024)
  is
    f : File_Type;
    buf : Byte_Buffer (1 .. buffer_size);
    actually_read : Natural;
  begin
    Open (f, In_File, file_name);
    loop
      Block_Read (f, buf, actually_read);
      exit when actually_read = 0;  --  this is expected
      Block_Write (into, buf (1 .. actually_read));
    end loop;
    Close (f);
  end Copy_File;

  procedure W (pdf : in out PDF_Out_Stream'Class; s : String) is
  pragma Inline (W);
  begin
    String'Write (pdf.pdf_stream, s);
  end W;

  NL : constant Character := ASCII.LF;

  procedure WL (pdf : in out PDF_Out_Stream'Class; s : String) is
  begin
    W (pdf, s & NL);
  end WL;

  procedure No_Nowhere (pdf : in out PDF_Out_Stream'Class) is
  begin
    if pdf.zone = nowhere then
      New_Page (pdf);
    end if;
  end No_Nowhere;

  --  Delayed output, for internal PDF's "stream" object

  procedure Write_delayed (pdf : in out PDF_Out_Stream'Class; s : String) is
  pragma Inline (Write_delayed);
  begin
    No_Nowhere (pdf);
    Append (pdf.stream_obj_buf, s);
  end Write_delayed;

  procedure WLd (pdf : in out PDF_Out_Stream'Class; s : String) is
  pragma Inline (WLd);
  begin
    Write_delayed (pdf, s & NL);
  end WLd;

  procedure Begin_Text (pdf : in out PDF_Out_Stream'Class) is
  begin
    WLd (pdf,  "  BT");  --  Begin Text object (9.4.1, Table 107)
  end Begin_Text;

  procedure End_Text (pdf : in out PDF_Out_Stream'Class) is
  begin
    WLd (pdf,  "  ET");
  end End_Text;

  procedure Flip_To (pdf : in out PDF_Out_Stream'Class; new_state : Text_or_Graphics) is
  begin
    No_Nowhere (pdf);
    --  WLd (pdf,  " % Text_or_graphics before: " & pdf.text_switch'Image);
    if pdf.text_switch /= new_state then
      pdf.text_switch := new_state;
      case new_state is
        when text     => Begin_Text (pdf);
        when graphics => End_Text (pdf);
      end case;
    end if;
    --  WLd (pdf,  " % Text_or_graphics after: " & pdf.text_switch'Image);
  end Flip_To;

  -----------------------------
  --  Direct code insertion  --
  -----------------------------

  procedure Insert_PDF_Code (pdf : in out PDF_Out_Stream; code : String) is
  begin
    WLd (pdf, "    " & code);  --  Indentation is just cosmetic...
  end Insert_PDF_Code;

  procedure Insert_Text_PDF_Code (pdf : in out PDF_Out_Stream; code : String) is
  begin
    Flip_To (pdf, text);
    Insert_PDF_Code (pdf, code);
  end Insert_Text_PDF_Code;

  procedure Insert_Graphics_PDF_Code (pdf : in out PDF_Out_Stream; code : String) is
  begin
    Flip_To (pdf, graphics);
    Insert_PDF_Code (pdf, code);
  end Insert_Graphics_PDF_Code;

  --  External stream index

  function Buffer_index (pdf : PDF_Out_Stream'Class) return Ada.Streams.Stream_IO.Count is
  begin
    return Index (pdf) - pdf.start_index;
  end Buffer_index;

  function Img (p : Integer) return String is
    s : constant String := Integer'Image (p);
  begin
    if p < 0 then
      return s;
    else
      return s (s'First + 1 .. s'Last);  --  Skip the *%"%! front space
    end if;
  end Img;

  function Img (p : PDF_Index_Type) return String is
    s : constant String := PDF_Index_Type'Image (p);
  begin
    if p < 0 then
      return s;
    else
      return s (s'First + 1 .. s'Last);  --  Skip the *%"%! front space
    end if;
  end Img;

  package RIO is new Ada.Text_IO.Float_IO (Real);

  --  Compact real number image, taken from TeXCAD (TeX_Number in tc.adb)
  --
  function Img (x : Real; prec : Positive := max_displayed_digits) return String is
    s : String (1 .. 20 + prec);
    na, nb, np : Natural;
  begin
    RIO.Put (s, x, prec, 0);
    na := s'First;
    nb := s'Last;
    np := 0;
    for i in s'Range loop
      case s (i) is
        when '.' => np := i; exit;    --   Find a decimal point
        when ' ' => na := i + 1;      -- * Trim spaces on left
        when others => null;
      end case;
    end loop;
    if np > 0 then
      while nb > np and then s (nb) = '0' loop
        nb := nb - 1;                 -- * Remove extra '0's
      end loop;
      if nb = np then
        nb := nb - 1;                 -- * Remove '.' if it is at the end
      elsif s (na .. np - 1) = "-0" then
        na := na + 1;
        s (na) := '-';                -- * Reduce "-0.x" to "-.x"
      elsif s (na .. np - 1) = "0" then
        na := na + 1;                 -- * Reduce "0.x" to ".x"
      end if;
    end if;
    return s (na .. nb);
  end Img;

  function "+"(P1, P2 : Point) return Point is
  begin
    return (P1.x + P2.x, P1.y + P2.y);
  end "+";

  function "*"(f : Real; P : Point) return Point is
  begin
    return (f * P.x, f * P.y);
  end "*";

  function L1_Distance (P1, P2 : Point) return Real is
  begin
    return abs (P1.x - P2.x) + abs (P1.y - P2.y);
  end L1_Distance;

  function L2_Distance_Squared (P1, P2 : Point) return Real is
  begin
    return (P1.x - P2.x) ** 2 + (P1.y - P2.y) ** 2;
  end L2_Distance_Squared;

  function "+"(P : Point; r : Rectangle) return Rectangle is
  begin
    return (P.x + r.x_min, P.y + r.y_min, r.width, r.height);
  end "+";

  function "*"(f : Real; r : Rectangle) return Rectangle is
  begin
    return (r.x_min, r.y_min, f * r.width, f * r.height);
  end "*";

  function X_Max (r : Rectangle) return Real is
  begin
    return r.x_min + r.width;
  end X_Max;

  function Y_Max (r : Rectangle) return Real is
  begin
    return r.y_min + r.height;
  end Y_Max;

  function Expand (r : Rectangle; left, right, up, down : Real) return Rectangle is
  ((x_min  => r.x_min - left,
    y_min  => r.y_min - down,
    width  => r.width + left + right,
    height => r.height + down + up));

  type Abs_Rel_Mode is (absolute, relative);

  function Img (p : Point) return String is
  begin
    return Img (p.x) & ' ' & Img (p.y);
  end Img;

  function Img (box : Rectangle; mode : Abs_Rel_Mode) return String is
  begin
    case mode is
      when absolute =>
        return Img (box.x_min) & ' ' & Img (box.y_min) & ' ' &
               Img (X_Max (box)) & ' ' & Img (Y_Max (box));
      when relative =>
        return Img (box.x_min) & ' ' & Img (box.y_min) & ' ' &
               Img (box.width) & ' ' & Img (box.height);
    end case;
  end Img;

  procedure Dispose is new Ada.Unchecked_Deallocation (Offset_Table, p_Offset_Table);

  procedure New_fixed_index_object (pdf : in out PDF_Out_Stream'Class; idx : PDF_Index_Type) is
    new_table : p_Offset_Table;
  begin
    if pdf.object_offset = null then
      pdf.object_offset := new Offset_Table (1 .. idx);
    elsif pdf.object_offset'Last < idx then
      new_table := new Offset_Table (1 .. idx * 2);
      new_table (1 .. pdf.object_offset'Last) := pdf.object_offset.all;
      Dispose (pdf.object_offset);
      pdf.object_offset := new_table;
    end if;
    pdf.object_offset (idx) := Buffer_index (pdf);
    WL (pdf, Img (idx) & " 0 obj");
  end New_fixed_index_object;

  procedure New_Object (pdf : in out PDF_Out_Stream'Class) is
  begin
    pdf.objects := pdf.objects + 1;
    New_fixed_index_object (pdf, pdf.objects);
  end New_Object;

  producer : constant String :=
    "Ada PDF Writer " & version & ", ref: " & reference & ", " & web &
    " , using GID (Generic Image Decoder) version " & GID.version;

  procedure Write_PDF_header (pdf : in out PDF_Out_Stream'Class) is
  begin
    pdf.is_created := True;
    pdf.start_index := Index (pdf);
    case pdf.format is
      when PDF_1_3 =>
        WL (pdf, "%PDF-1.3");
        Byte_Buffer'Write (pdf.pdf_stream, (16#25#, 16#C2#, 16#A5#, 16#C2#, 16#B1#, 16#C3#, 16#AB#, 10));
    end case;
    WL (pdf, "%  --  Produced by " & producer);
  end Write_PDF_header;

  procedure New_Substream (pdf : in out PDF_Out_Stream'Class) is
  begin
    pdf.stream_obj_buf := Null_Unbounded_String;
  end New_Substream;

  procedure Finish_Substream (pdf : in out PDF_Out_Stream'Class) is
    chunk_size : constant := 1024;
    len, start, stop : Natural;
  begin
    len := Length (pdf.stream_obj_buf);
    WL (pdf, "  << /Length " & Img (len) & " >>");
    --  The length could be alternatively stored in the next PDF object,
    --  so we wouldn't need to buffer the stream - see 7.3.10, Example 3.
    --  But we prefer the buffered version, which could be compressed, in
    --  a future version of this package.
    WL (pdf, "stream");
    if len > 0 then
      --  Alternative to using To_String (pdf.stream_obj_buf): we
      --  output the buffer sliced in small chunks.
      --  Reason: the buffer can be very large and a call to To_String
      --  may cause a stack overflow.
      start := 1;
      loop
        stop := Integer'Min (len, start + chunk_size - 1);
        W (pdf, Slice (pdf.stream_obj_buf, start, stop));
        start := stop + 1;
        exit when start > len;
      end loop;
    end if;
    WL (pdf, "");
    WL (pdf, "endstream");
  end Finish_Substream;

  --  Internal - test page for experimenting PDF constructs (and how Adobe Reader reacts to them)
  --
  procedure Test_Page (pdf : in out PDF_Out_Stream'Class) is
  begin
    WLd (pdf, "10 10 200 200 re S"); -- rectangle, stroke
    WLd (pdf, "  BT");            --  Begin Text object (9.4). Text matrix and text line matrix:= I
    WLd (pdf, "    /Ada_PDF_Std_Font_Helvetica 24 Tf");   --  F1 font, 24 pt size (9.3 Text State Parameters and Operators)
    WLd (pdf, "    0.5 0 0 rg");  --  red, nonstroking colour (Table 74)
    WLd (pdf, "    0.25 G");     --  25% gray stroking colour (Table 74)
    WLd (pdf, "    2 Tr");        --  Tr: Set rendering mode as "Fill, then stroke text" (Table 106)
    WLd (pdf, "    20 539 Td");
    WLd (pdf, "    (Hello World !) Tj"); -- Tj: Show a text string (9.4.3 Text-Showing Operators)
    WLd (pdf, "    16 TL");       --  TL: set text leading (distance between lines, 9.3.5)
    WLd (pdf, "    T*");          --  T*: Move to the start of the next line (9.4.2)
    WLd (pdf, "    20 20 200 200 re S"); -- rectangle, stroke (within text region)
    WLd (pdf, "    /Ada_PDF_Std_Font_Helvetica-Oblique 12 Tf");
    WLd (pdf, "    0 Tr");        --  Tr: Set rendering mode as default: "Fill text" (Table 106)
    WLd (pdf, "    0 g");         --  black (default)
    WLd (pdf, "    (Subtitle here.) Tj T*");
    WLd (pdf, "  ET");           --  End Text
    WLd (pdf, "30 30 200 200 re S"); -- rectangle, stroke
    WLd (pdf, "  BT");
    WLd (pdf, "    5 5 Td (Second text chunk here.) Tj T*");
    WLd (pdf, "  ET");
    WLd (pdf, "40 40 240 240 re S"); -- rectangle, stroke
    WLd (pdf, "15 15 Td (Text chunk not within BT/ET.) Tj");
  end Test_Page;

  test_page_mode : constant Boolean := False;

  procedure Insert_PDF_Font_Selection_Code (pdf : in out PDF_Out_Stream) is
  begin
    Insert_Text_PDF_Code
      (pdf,
       PDF_Out.Fonts.Current_Font_Dictionary_Name (pdf) &
       ' ' & Img (pdf.font_size) & " Tf " &              --  Tf: 9.3 Text State Parameters and Operators
       Img (pdf.font_size * pdf.line_spacing) & " TL");  --  TL: set text leading (9.3.5)

    if pdf.current_font in Standard_Font_Type then
      pdf.std_font_used_in_page (pdf.current_font) := True;
    end if;
  end Insert_PDF_Font_Selection_Code;

  procedure Font (pdf : in out PDF_Out_Stream; f : Standard_Font_Type) is
  begin
    pdf.current_font := f;
    Insert_PDF_Font_Selection_Code (pdf);
  end Font;

  procedure Font_Size (pdf : in out PDF_Out_Stream; size : Real) is
  begin
    pdf.font_size := size;
    Insert_PDF_Font_Selection_Code (pdf);
  end Font_Size;

  function Get_Font_Size (pdf : PDF_Out_Stream) return Real is
  begin
    return pdf.font_size;
  end Get_Font_Size;

  function Bounding_Box (pdf : PDF_Out_Stream; text : String) return Rectangle is
  begin
    if pdf.current_font in Standard_Font_Type then
      return
        (x_min  => 0.0,
         y_min  => Fonts.Vertical_Offset (pdf.current_font, pdf.font_size),
         width  => Fonts.Width (pdf.current_font, pdf.font_size, text),
         height => pdf.font_size);
    else
      raise Not_implemented with "So far only standard fonts are supported";
    end if;
  end Bounding_Box;

  procedure Line_Spacing (pdf : in out PDF_Out_Stream; factor : Real) is
  begin
    pdf.line_spacing := factor;
    Insert_PDF_Font_Selection_Code (pdf);
  end Line_Spacing;

  procedure Line_Spacing_Pt (pdf : in out PDF_Out_Stream; pt : Real) is
  begin
    pdf.line_spacing := pt / pdf.font_size;
    --  !! This assumes that the font size is in Point (pt) units.
    Insert_PDF_Font_Selection_Code (pdf);
  end Line_Spacing_Pt;

  procedure Dispose is new Ada.Unchecked_Deallocation (Page_Table, p_Page_Table);

  procedure New_Page (pdf : in out PDF_Out_Stream) is
    new_table : p_Page_Table;
  begin
    if pdf.zone /= nowhere then
      Finish_Page (pdf);
    end if;
    pdf.last_page := pdf.last_page + 1;
    pdf.current_line  := 1;
    pdf.current_col   := 1;
    pdf.current_annot := Null_Unbounded_String;
    pdf.std_font_used_in_page := (others => False);
    PDF_Out.Images.Clear_local_resource_flags (pdf);
    --
    --  Page descriptor object:
    --
    New_Object (pdf);
    if pdf.page_idx = null then
      pdf.page_idx := new Page_Table (1 .. pdf.last_page);
    elsif pdf.page_idx'Last < pdf.last_page then
      new_table := new Page_Table (1 .. pdf.last_page * 2);
      new_table (1 .. pdf.page_idx'Last) := pdf.page_idx.all;
      Dispose (pdf.page_idx);
      pdf.page_idx := new_table;
    end if;
    pdf.page_idx (pdf.last_page) := pdf.objects;
    --  Table 30 (7.7.3.3 Page Objects) for options
    WL (pdf, "  <</Type /Page");
    WL (pdf, "    /Parent " & Img (pages_idx) & " 0 R");
    --  Contents stream object is object number n+1 (our choice):
    WL (pdf, "    /Contents "  & Img (pdf.objects + 1) & " 0 R");
    --  Annotations. Annotations array is object number n+2 (our choice):
    WL (pdf, "    /Annots "    & Img (pdf.objects + 2) & " 0 R");
    --  Resources: a dictionary containing any resources required by the page.
    --  Resources object is object number n+3 (our choice):
    WL (pdf, "    /Resources " & Img (pdf.objects + 3) & " 0 R");
    WL (pdf, "    /MediaBox [" & Img (pdf.page_box, absolute) & ']');
    WL (pdf, "  >>");
    WL (pdf, "endobj");
    --  Page contents object:
    --
    New_Object (pdf);
    New_Substream (pdf);
    if test_page_mode then
      Test_Page (pdf);
    else
      pdf.zone := in_page;
      Insert_PDF_Font_Selection_Code (pdf);
      pdf.zone := in_header;
      --  PDF_Out_Stream'Class: make the call to Page_Header dispatching
      Page_Header (PDF_Out_Stream'Class (pdf));
    end if;
    pdf.zone := in_page;
    Text_XY (pdf, pdf.page_margins.left, Y_Max (pdf.page_box) - pdf.page_margins.top);
  end New_Page;

  procedure Finish_Page (pdf : in out PDF_Out_Stream) is

    appended_object_idx : PDF_Index_Type;

    procedure Annotations is
    begin
      New_Object (pdf);
      WL (pdf, "[");  --  It is an array (of annotations).
      W  (pdf, To_String (pdf.current_annot));
      WL (pdf, "]");
      WL (pdf, "endobj");
    end Annotations;

    procedure Resources is

      procedure Image_Item (dn : in out Dir_Node) is
        img_obj : PDF_Index_Type;
      begin
        if dn.local_resource then
          if dn.pdf_object_index = 0 then
            img_obj := appended_object_idx;
            appended_object_idx := appended_object_idx + 1;
          else
            img_obj := dn.pdf_object_index;  --  image has been loaded for a previous page
          end if;
          WL (pdf, Image_Name (dn.image_index) & ' ' & Img (img_obj) & " 0 R");
        end if;
      end Image_Item;

      procedure Image_List is new PDF_Out.Images.Traverse_private (Image_Item);

    begin
      New_Object (pdf);
      WL (pdf, "<<");
      --  Font resources:
      PDF_Out.Fonts.Font_Dictionary (pdf);
      appended_object_idx := pdf.objects + 1;  --  Images contents to be appended after this object
      --  Image resources:
      WL (pdf, "  /XObject <<");
      Image_List (pdf);
      WL (pdf, "  >>");
      WL (pdf, ">>");
      WL (pdf, "endobj");
    end Resources;

  begin
    if pdf.zone = nowhere then
      return;  --  We are already "between pages"
    end if;
    if test_page_mode then
      null;  --  Nothing to do anymore with test page
    else
      pdf.zone := in_footer;
      --  PDF_Out_Stream'Class: make the call to Page_Header dispatching
      Page_Footer (PDF_Out_Stream'Class (pdf));
      Flip_To (pdf, graphics);
    end if;
    pdf.zone := nowhere;
    Finish_Substream (pdf);
    WL (pdf, "endobj");  --  end of page contents.

    --  Annotations (7.7.3.3) for the page just finished:
    Annotations;

    --  Resources Dictionary (7.8.3) for the page just finished:
    Resources;

    PDF_Out.Images.Insert_unloaded_local_images (pdf);
  end Finish_Page;

  procedure Put (pdf  : in out PDF_Out_Stream;
                 num  : in Real;
                 fore : in Ada.Text_IO.Field := Real_IO.Default_Fore;
                 aft  : in Ada.Text_IO.Field := Real_IO.Default_Aft;
                 exp  : in Ada.Text_IO.Field := Real_IO.Default_Exp)
  is
  begin
    if exp = 0 then
      declare
        s : String (1 .. fore + 1 + aft);  --  "  123.45"
      begin
        Real_IO.Put (s, num, aft, exp);
        Put (pdf, s);
      end;
    else
      declare
        s : String (1 .. fore + 1 + aft + 1 + exp);  --  "  1.234E-01"
      begin
        Real_IO.Put (s, num, aft, exp);
        Put (pdf, s);
      end;
    end if;
  end Put;

  procedure Put (pdf   : in out PDF_Out_Stream;
                 num   : in Integer;
                 width : in Ada.Text_IO.Field       := 0;  --  ignored
                 base  : in Ada.Text_IO.Number_Base := 10)
  is
  begin
    if base = 10 then
      Put (pdf, Img (num));
    else
      declare
        use Ada.Strings.Fixed;
        s : String (1 .. 50 + 0 * width);
        --  "0*width" is just to skip a warning about width being unused
        package IIO is new Ada.Text_IO.Integer_IO (Integer);
      begin
        IIO.Put (s, num, Base => base);
        Put (pdf, Trim (s, Ada.Strings.Left));
      end;
    end if;
  end Put;

  procedure Show_Text_String (pdf : in out PDF_Out_Stream; str : String) is
  --  9.4.3 Text-Showing Operators; table 109.
  begin
    if str'Length > 0 then
      Insert_Text_PDF_Code (pdf, '(' & str & ") Tj");
    end if;
  end Show_Text_String;

  procedure Put (pdf : in out PDF_Out_Stream; str : String) is
  begin
    if test_page_mode then
      null;  --  Nothing to do (test page instead)
    else
      for i in str'Range loop
        --  We scan the string for special characters:
        case str (i) is
          when ASCII.NUL .. ASCII.HT |
               ASCII.VT .. ASCII.US =>
            --  Skip special character.
            Show_Text_String (pdf, str (str'First .. i - 1));
            Put (pdf, str (i + 1 .. str'Last));
            return;
          when ASCII.LF =>
            --  Line Feed character: display string on two or more lines.
            Show_Text_String (pdf, str (str'First .. i - 1));
            New_Line (pdf);
            Put (pdf, str (i + 1 .. str'Last));
            return;
          when '(' | ')' | '\' =>
            --  Insert a Reverse Solidus (backslash, '\') for an escape
            --  sequence. See full list in: 7.3.4.2 Literal Strings; table 3.
            Show_Text_String (pdf, str (str'First .. i - 1) & '\' & str (i));
            Put (pdf, str (i + 1 .. str'Last));
            return;
          when others =>
            null;
        end case;
      end loop;
      Show_Text_String (pdf, str);
    end if;
  end Put;

  procedure Put (pdf : in out PDF_Out_Stream; str : Unbounded_String) is
  begin
    Put (pdf, To_String (str));
  end Put;

  procedure Put_Line (pdf  : in out PDF_Out_Stream;
                      num  : in Real;
                      fore : in Ada.Text_IO.Field := Real_IO.Default_Fore;
                      aft  : in Ada.Text_IO.Field := Real_IO.Default_Aft;
                      exp  : in Ada.Text_IO.Field := Real_IO.Default_Exp)
  is
  begin
    Put (pdf, num, fore, aft, exp);
    New_Line (pdf);
  end Put_Line;

  procedure Put_Line (pdf   : in out PDF_Out_Stream;
                      num   : in Integer;
                      width : in Ada.Text_IO.Field := 0;  --  ignored
                      base  : in Ada.Text_IO.Number_Base := 10)
  is
  begin
    Put (pdf, num, width, base);
    New_Line (pdf);
  end Put_Line;

  procedure Put_Line (pdf : in out PDF_Out_Stream; str : String) is
  begin
    Put (pdf, str);
    New_Line (pdf);
  end Put_Line;

  procedure Put_Line (pdf : in out PDF_Out_Stream; str : Unbounded_String) is
  begin
    Put_Line (pdf, To_String (str));
  end Put_Line;

  procedure New_Line (pdf : in out PDF_Out_Stream; Spacing : Positive := 1) is
  begin
    pdf.current_line := pdf.current_line + 1;
    pdf.current_col := 1;
    if test_page_mode then
      null;  --  Nothing to do (test page instead)
    else
      for i in 1 .. Spacing loop
        Insert_Text_PDF_Code (pdf, "T*");
      end loop;
    end if;
  end New_Line;

  procedure Put_WS
    (pdf : in out PDF_Out_Stream; w_str : Wide_String) is
    use Ada.Characters.Conversions;
  begin
     Put (pdf, To_String (w_str));
  end Put_WS;

  procedure Put_Line_WS
    (pdf : in out PDF_Out_Stream; w_str : Wide_String)
  is
    use Ada.Characters.Conversions;
  begin
    Put_Line (pdf, To_String (w_str));
  end Put_Line_WS;

  procedure Put_Line_WS
     (pdf   : in out PDF_Out_Stream;
      w_str :        Ada.Strings.Wide_Unbounded.Unbounded_Wide_String)
  is
    use Ada.Characters.Conversions, Ada.Strings.Wide_Unbounded;
  begin
     Put_Line (pdf, To_String (To_Wide_String (w_str)));
  end Put_Line_WS;

  procedure Put_WWS
    (pdf : in out PDF_Out_Stream; ww_str : Wide_Wide_String) is
    use Ada.Characters.Conversions;
  begin
     Put (pdf, To_String (ww_str));
  end Put_WWS;

  procedure Put_Line_WWS
    (pdf : in out PDF_Out_Stream; ww_str : Wide_Wide_String)
  is
    use Ada.Characters.Conversions;
  begin
    Put_Line (pdf, To_String (ww_str));
  end Put_Line_WWS;

  procedure Put_Line_WWS
     (pdf    : in out PDF_Out_Stream;
      ww_str :        Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String)
  is
    use Ada.Characters.Conversions, Ada.Strings.Wide_Wide_Unbounded;
  begin
     Put_Line (pdf, To_String (To_Wide_Wide_String (ww_str)));
  end Put_Line_WWS;

  procedure Text_XY (pdf : in out PDF_Out_Stream; x, y : Real) is
  begin
    Flip_To (pdf, text);
    --  The following explicit End_text, Begin_text are just
    --  for resetting the text matrices (hence, position and orientation).
    End_Text (pdf);
    Begin_Text (pdf);
    Insert_PDF_Code (pdf, Img (x) & ' ' & Img (y) & " Td");  --  Td: 9.4.2 Text-Positioning Operators
    pdf.current_line := 1;
    pdf.current_col := 1;
  end Text_XY;

  procedure Put_XY (pdf : in out PDF_Out_Stream; x, y : Real; str : String) is
  begin
    Text_XY (pdf, x, y);
    Put (pdf, str);
  end Put_XY;

  procedure Put_XY (pdf : in out PDF_Out_Stream; pt : Point; str : String) is
  begin
    Put_XY (pdf, pt.x, pt.y, str);
  end Put_XY;

  function Col (pdf : in PDF_Out_Stream) return Positive is
  begin
    return pdf.current_col;
  end Col;

  function Line (pdf : in PDF_Out_Stream) return Positive is
  begin
    return pdf.current_line;
  end Line;

  function Page (pdf : in PDF_Out_Stream) return Natural is
  begin
    return Natural (pdf.last_page);  --  Issue if Integer is 16-bit and last_page > 2**15-1
  end Page;

  procedure Color (pdf : in out PDF_Out_Stream; c : Color_Type) is
  begin
    Insert_PDF_Code (pdf, Img (c.red) & ' ' & Img (c.green) & ' ' & Img (c.blue) & " rg");
    --  rg = nonstroking colour (Table 74)
  end Color;

  procedure Stroking_Color (pdf : in out PDF_Out_Stream; c : Color_Type) is
  begin
    Insert_PDF_Code (pdf, Img (c.red) & ' ' & Img (c.green) & ' ' & Img (c.blue) & " RG");
    --  RG = nonstroking colour (Table 74)
  end Stroking_Color;

  procedure Text_Rendering_Mode (pdf : in out PDF_Out_Stream; r : Rendering_Mode) is
  begin
    Insert_Text_PDF_Code (pdf, Img (Integer (Rendering_Mode'Pos (r))) & " Tr");
    --  Tr = Set rendering mode (Table 106)
  end Text_Rendering_Mode;

  function Convert (rgb_code : RGB_Code_Range) return Color_Type is
  begin
    return
      (red   =>           Real (rgb_code mod 256) / 255.0,
       green =>   Real ((rgb_code / 256) mod 256) / 255.0,
       blue  => Real ((rgb_code / 65536) mod 256) / 255.0);
  end Convert;

  function Image_Name (i : Positive) return String is
  begin
    return "/Ada_PDF_Img" & Img (i);
  end Image_Name;

  procedure Image (pdf : in out PDF_Out_Stream; file_name : String; target : Rectangle) is
    image_index : Positive;  --  Index in the list of images
  begin
    No_Nowhere (pdf);
    PDF_Out.Images.Image_ref (pdf, file_name, image_index);
    Insert_Graphics_PDF_Code
      (pdf, "q " &
       Img (target.width) & " 0 0 " & Img (target.height) &
       ' ' & Img (target.x_min) & ' ' & Img (target.y_min) & " cm " &  --  cm: Table 57
       Image_Name (image_index) & " Do Q");
  end Image;

  function Get_Pixel_Dimensions (image_file_name : String) return Rectangle is
  begin
    return PDF_Out.Images.Get_pixel_dimensions (image_file_name);
  end Get_Pixel_Dimensions;

  -----------------------
  --  Vector graphics  --
  -----------------------

  function Almost_Zero (x : Real) return Boolean is
  begin
    return abs x <= Real'Base'Model_Small;
  end Almost_Zero;

  procedure Line_Width (pdf : in out PDF_Out_Stream; width : Real) is
  begin
    Insert_Graphics_PDF_Code (pdf, Img (width) & " w");
  end Line_Width;

  procedure Single_Line (pdf : in out PDF_Out_Stream; from, to : Point) is
  begin
    Insert_Graphics_PDF_Code (pdf, Img (from) & " m " & Img (to) & " l s");
  end Single_Line;

  --    Table 59 - Path Construction Operators (8.5.2)
  --    Table 60 - Path-Painting Operators (8.5.3.1)

  inside_path_rule_char : constant array (Inside_path_rule) of Character :=
    (nonzero_winding_number => ' ',
     even_odd               => '*');

  path_drawing_operator : constant array (Path_Rendering_Mode) of Character :=
    (fill             => 'F',
     stroke           => 'S',
     fill_then_stroke => 'B');

  procedure Draw (pdf : in out PDF_Out_Stream; what : Rectangle; rendering : Path_Rendering_Mode) is
  begin
    Insert_Graphics_PDF_Code (pdf, Img (what, relative) & " re " & path_drawing_operator (rendering));
  end Draw;

  procedure Move (pdf : in out PDF_Out_Stream; to : Point) is
  begin
    Insert_Graphics_PDF_Code (pdf, Img (to) & " m");  --  m operator (Table 59)
  end Move;

  procedure Line (pdf : in out PDF_Out_Stream; to : Point) is
  begin
    Insert_Graphics_PDF_Code (pdf, Img (to) & " l");
  end Line;

  procedure Cubic_Bezier
    (pdf                  : in out PDF_Out_Stream;
     control_1, control_2 : in     Point;
     to                   : in     Point)
  is
  begin
    Insert_Graphics_PDF_Code
      (pdf,
       Img (control_1) & ' ' &
       Img (control_2) & ' ' &
       Img (to) & " c");
  end Cubic_Bezier;

  procedure Arc
    (pdf              : in out PDF_Out_Stream;
     center           : in     Point;
     radius           : in     Real;
     angle_1, angle_2 : in     Real;
     line_to_start    : in     Boolean)
  is
    --  Improved from Pascal code shown here:
    --  https://stackoverflow.com/questions/26764342/drawing-arc-with-bezier-curves
    --
    use Ada.Numerics, Real_Elementary_Functions;
    sweep_angle : Real;
    n_curves : Integer;
    deg_to_rad  : constant := Pi / 180.0;
    angle_start : constant Real := angle_1 * deg_to_rad;
    angle_stop  : constant Real := angle_2 * deg_to_rad;
    sn, cs, sweep_angle_part, angle_part_start : Real;
    p, q : array (0 .. 3) of Point;
    v, t, q0 : Point;

    --  The following value looks like an optimum (it possibly
    --  minimizes the distance from the curve to the actual arc):
    scale : constant := 4.0 / 3.0;
  begin
    sweep_angle := angle_stop - angle_start;

    if Almost_Zero (sweep_angle) then
      return;
    end if;

    --  If sweep_angle is too large, divide arc to smaller ones:
    n_curves := Integer (Real'Ceiling (abs (sweep_angle) / (Pi * 0.5)));

    sweep_angle_part := sweep_angle / Real (n_curves);

    --  Calculates control points for Bezier approximation of
    --  an arc with radius=1, circle center at (0,0),
    --  middle of the arc at (1,0).

    v.x := Cos (sweep_angle_part * 0.5);
    v.y := Sin (sweep_angle_part * 0.5);

    t.x := (1.0 - v.x) * scale;
    t.y := -t.x * v.x / v.y;  --  Vectors t and v must be perpendicular.

    --  End point 2
    p (3) := v;
    --  Control point 2
    p (2) := v + t;
    --  Control point 1
    p (1) := (+p (2).x, -p (2).y);
    --  End point 1
    p (0) := (+p (3).x, -p (3).y);

    --  Rotation and translation of control points,
    --  the start point and the end points.

    cs := Cos (angle_start + sweep_angle_part * 0.5);
    sn := Sin (angle_start + sweep_angle_part * 0.5);

    q0 := center +
            radius * (p (0).x * cs - p (0).y * sn,
                      p (0).x * sn + p (0).y * cs);

    if line_to_start then
      Line (pdf, q0);
    else
      Move (pdf, q0);
    end if;

    for i_curve in 1 .. n_curves loop
      angle_part_start := angle_start + sweep_angle_part * Real (i_curve - 1);

      sn := Sin (angle_part_start + sweep_angle_part * 0.5);
      cs := Cos (angle_part_start + sweep_angle_part * 0.5);

      for i in 1 .. 3 loop
        q (i) :=
          center +
            radius * (p (i).x * cs - p (i).y * sn,
                      p (i).x * sn + p (i).y * cs);
      end loop;

      Cubic_Bezier (pdf, q (1), q (2), q (3));
    end loop;
  end Arc;

  procedure Circle
    (pdf       : in out PDF_Out_Stream;
     center    : in     Point;
     radius    : in     Real;
     rendering : in     Path_Rendering_Mode)
  is
  begin
    Arc (pdf, center, radius, 0.0, 360.0, False);
    Finish_Path (pdf, False, rendering, nonzero_winding_number);
  end Circle;

  procedure Finish_Path
    (pdf        : in out PDF_Out_Stream;
     close_path :        Boolean;
     rendering  :        Path_Rendering_Mode;  --  fill, stroke, or both
     rule       :        Inside_path_rule)
  is
    cmd : String := path_drawing_operator (rendering) & inside_path_rule_char (rule);
  begin
    if close_path then
      cmd := Ada.Characters.Handling.To_Lower (cmd);
    end if;
    if cmd in "s*" | "S*" | "F*" then
      --  Undefined operator.
      --  End the path object without filling or stroking it.
      Insert_Graphics_PDF_Code (pdf, "n");
    elsif cmd = "F " then
      --  "Equivalent to f; included only for compatibility.
      --   Although PDF reader applications shall be able to accept
      --   this operator, PDF writer applications should use f instead."
      Insert_Graphics_PDF_Code (pdf, "f");
    else
      --  Insert the s, S, f, f*, F, b, b*, B, B* of
      --  Table 60 - Path-Painting Operators (8.5.3.1)
      Insert_Graphics_PDF_Code (pdf, cmd);
    end if;
  end Finish_Path;

  procedure Hyperlink
    (pdf     : in out PDF_Out_Stream;
     area    : in     Rectangle;
     visible : in     Boolean;
     url     : in     String)
  is
  begin
    Append
      (pdf.current_annot,
       "  << /Type /Annot /Subtype /Link /Rect [" &
       Img (area, absolute) &
       "] " & (if visible then "" else "/C []") &
       " /A << /Type /Action /S /URI /URI (" &
       url &
       ") >> >>" &
       NL);
  end Hyperlink;

  link_margin : constant := 4.0;  --  Margin around a textual link.

  procedure Hyperlink
    (pdf     : in out PDF_Out_Stream;
     origin  : in     Point;
     text    : in     String;
     visible : in     Boolean;
     url     : in     String)
  is
  begin
    Put_XY (pdf, origin.x, origin.y, text);
    Hyperlink
      (pdf,
       origin + Expand (Bounding_Box (pdf, text), link_margin, link_margin, link_margin, link_margin),
       visible,
       url);
  end Hyperlink;

  procedure Hyperlink
    (pdf     : in out PDF_Out_Stream;
     area    : in     Rectangle;
     visible : in     Boolean;
     page    : in     Positive;
     y_pos   : in     Integer := unspecified_position)
  is

  begin
    if pdf.old_page_idx = null then
      null;  --  You need to produce the same PDF document twice!
    elsif PDF_Index_Type (page) not in pdf.old_page_idx'Range then
      null;  --  Out of range
    else
      Append
        (pdf.current_annot,
         "  << /Type /Annot /Subtype /Link /Rect [" &
         Img (area, absolute) &
         "] " & (if visible then "" else "/C []") &
         " /Dest [" & Img (pdf.old_page_idx (PDF_Index_Type (page))) &
         " 0 R /XYZ null" &
         (if y_pos = unspecified_position then " null" else y_pos'Image) &
         " null ] >>" &
         NL);
    end if;
  end Hyperlink;

  procedure Hyperlink
    (pdf     : in out PDF_Out_Stream;
     origin  : in     Point;
     text    : in     String;
     visible : in     Boolean;
     page    : in     Positive;
     y_pos   : in     Integer := unspecified_position)
  is
  begin
    Put_XY (pdf, origin.x, origin.y, text);
    Hyperlink
      (pdf,
       origin + Expand (Bounding_Box (pdf, text), link_margin, link_margin, link_margin, link_margin),
       visible,
       page,
       y_pos);
  end Hyperlink;

  --  Table 317 - Entries in the document information dictionary (14.3.3)

  procedure Title (pdf : in out PDF_Out_Stream; s : String) is
  begin
    pdf.doc_title := To_Unbounded_String (s);
  end Title;

  procedure Author (pdf : in out PDF_Out_Stream; s : String) is
  begin
    pdf.doc_author := To_Unbounded_String (s);
  end Author;

  procedure Subject (pdf : in out PDF_Out_Stream; s : String) is
  begin
    pdf.doc_subject := To_Unbounded_String (s);
  end Subject;

  procedure Keywords (pdf : in out PDF_Out_Stream; s : String) is
  begin
    pdf.doc_keywords := To_Unbounded_String (s);
  end Keywords;

  procedure Creator_Application (pdf : in out PDF_Out_Stream; s : String) is
  begin
    pdf.doc_creator := To_Unbounded_String (s);
  end Creator_Application;

  procedure Page_Header (pdf : in out PDF_Out_Stream) is
  begin
    null;  --  Default header is empty.
  end Page_Header;

  procedure Page_Footer (pdf : in out PDF_Out_Stream) is
  begin
    null;  --  Default footer is empty.
  end Page_Footer;

  procedure Left_Margin (pdf : out PDF_Out_Stream; pts : Real) is
  begin
    pdf.page_margins.left := pts;
  end Left_Margin;

  function Left_Margin (pdf : PDF_Out_Stream) return Real is
  begin
    return pdf.page_margins.left;
  end Left_Margin;

  procedure Right_Margin (pdf : out PDF_Out_Stream; pts : Real) is
  begin
    pdf.page_margins.right := pts;
  end Right_Margin;

  function Right_Margin (pdf : PDF_Out_Stream) return Real is
  begin
    return pdf.page_margins.right;
  end Right_Margin;

  procedure Top_Margin (pdf : out PDF_Out_Stream; pts : Real) is
  begin
    pdf.page_margins.top := pts;
  end Top_Margin;

  function Top_Margin (pdf : PDF_Out_Stream) return Real is
  begin
    return pdf.page_margins.top;
  end Top_Margin;

  procedure Bottom_Margin (pdf : out PDF_Out_Stream; pts : Real) is
  begin
    pdf.page_margins.bottom := pts;
  end Bottom_Margin;

  function Bottom_Margin (pdf : PDF_Out_Stream) return Real is
  begin
    return pdf.page_margins.bottom;
  end Bottom_Margin;

  procedure Margins (pdf : out PDF_Out_Stream; new_margins : Margins_Type) is
  begin
    pdf.page_margins := new_margins;
  end Margins;

  function Margins (pdf : PDF_Out_Stream) return Margins_Type is
  begin
    return pdf.page_margins;
  end Margins;

  procedure Page_Setup (pdf : in out PDF_Out_Stream; layout : Rectangle) is
    mb_x_min, mb_y_min, mb_x_max, mb_y_max : Real;
  begin
    pdf.page_box := layout;
    mb_x_min := Real'Min (pdf.maximum_box.x_min, layout.x_min);
    mb_y_min := Real'Min (pdf.maximum_box.y_min, layout.y_min);
    mb_x_max := Real'Max (X_Max (pdf.maximum_box), X_Max (layout));
    mb_y_max := Real'Max (Y_Max (pdf.maximum_box), Y_Max (layout));
    pdf.maximum_box :=
      (x_min  => mb_x_min,
        y_min  => mb_y_min,
        width  => mb_x_max - mb_x_min,
        height => mb_y_max - mb_y_min);
  end Page_Setup;

  function Layout (pdf : PDF_Out_Stream) return Rectangle is
  begin
    return pdf.page_box;
  end Layout;

  procedure Reset
    (pdf        : in out PDF_Out_Stream'Class;
     PDF_format : in     PDF_Type := default_PDF_type)
  is
    dummy_pdf_with_defaults : PDF_Out_Pre_Root_Type;
    mem_old_page_index : constant p_Page_Table := pdf.old_page_idx;
  begin
    --  Check if we are trying to re-use a half-finished object (ouch!):
    if pdf.is_created and not pdf.is_closed then
      raise PDF_stream_not_closed;
    end if;
    --  We will reset everything with defaults, except this:
    dummy_pdf_with_defaults.format := PDF_format;
    --  Now we reset pdf:
    PDF_Out_Pre_Root_Type (pdf) := dummy_pdf_with_defaults;
    --  ... but we restore the old page index:
    pdf.old_page_idx := mem_old_page_index;
    --  Set a default title (replaced when procedure Title is called).
    --  In Adobe Reader, this content can be copied to the clipboard.
    pdf.doc_title := "Document created with: " & To_Unbounded_String (producer);
  end Reset;

  procedure Finish (pdf : in out PDF_Out_Stream) is

    info_idx, cat_idx : PDF_Index_Type;

    procedure Info is
    begin
      New_Object (pdf);
      info_idx := pdf.objects;
      WL (pdf, "  << /Producer (" & producer & ')');
      WL (pdf, "     /Title (" & To_String (pdf.doc_title) & ')');
      WL (pdf, "     /Author (" & To_String (pdf.doc_author) & ')');
      WL (pdf, "     /Subject (" & To_String (pdf.doc_subject) & ')');
      WL (pdf, "     /Keywords (" & To_String (pdf.doc_keywords) & ')');
      WL (pdf, "     /Creator (" & To_String (pdf.doc_creator) & ')');
      WL (pdf, "  >>");
      WL (pdf, "endobj");
    end Info;

    procedure Pages_Dictionary is
    begin
      New_fixed_index_object (pdf, pages_idx);
      WL (pdf, "  << /Type /Pages");
      W (pdf,  "     /Kids [");
      for p in 1 .. pdf.last_page loop
        W (pdf, Img (pdf.page_idx (p)) & " 0 R ");
      end loop;
      WL (pdf, "]");
      if pdf.last_page > 0 then
        WL (pdf, "     /Count " & Img (pdf.last_page));
      end if;
      WL (pdf, "     /MediaBox [" & Img (pdf.maximum_box, absolute) & ']');

      --  7.7.3.3 Page Objects - MediaBox
      --  Boundaries of the physical medium on which the page shall be displayed or printed
      --  7.7.3.4 Inheritance of Page Attributes
      --  Global page size, lower-left to upper-right, measured in points
      --  Bounding box of all pages
      WL (pdf, "  >>");
      WL (pdf, "endobj");
    end Pages_Dictionary;

    procedure Catalog_Dictionary is
    begin
      New_Object (pdf);
      cat_idx := pdf.objects;
      WL (pdf, "  << /Type /Catalog");
      WL (pdf, "     /Pages " & Img (pages_idx) & " 0 R");
      if pdf.last_page > 0 then
        --  Open the document on page 1, fit the
        --  entire page within the window (Table 151):
        WL (pdf, "     /OpenAction [" & Img (pdf.page_idx (1)) & " 0 R /Fit]");
      end if;
      WL (pdf, "  >>");
      WL (pdf, "endobj");
    end Catalog_Dictionary;

    procedure Trailer is
    begin
      WL (pdf, "trailer");
      WL (pdf, "  << /Root " & Img (cat_idx) & " 0 R");
      WL (pdf, "     /Size " & Img (pdf.objects + 1));
      WL (pdf, "     /Info " & Img (info_idx) & " 0 R");
      WL (pdf, "  >>");
    end Trailer;

    xref_offset : Ada.Streams.Stream_IO.Count;

    procedure XRef is
      s10 : String (1 .. 10);
    begin
      xref_offset := Buffer_index (pdf);
      WL (pdf, "xref");
      WL (pdf, "0 " & Img (pdf.objects + 1));
      WL (pdf, "0000000000 65535 f ");
      for i in 1 .. pdf.objects loop
        CIO.Put (s10, pdf.object_offset (i));
        for n in s10'Range loop
          if s10 (n) = ' ' then
            s10 (n) := '0';
          end if;
        end loop;
        WL (pdf, s10 & " 00000 n ");  --   <-- the trailing space is needed!
      end loop;
    end XRef;

  begin
    if pdf.last_page = 0 then
      --  No page ? Then make quickly a blank page.
      New_Page (pdf);
    end if;
    Finish_Page (pdf);
    Info;
    Pages_Dictionary;
    Catalog_Dictionary;
    XRef;
    Trailer;
    WL (pdf, "startxref"); -- offset of xref
    WL (pdf, Img (Integer (xref_offset)));
    WL (pdf, "%%EOF");
    Dispose (pdf.old_page_idx);
    pdf.old_page_idx := pdf.page_idx;
    Dispose (pdf.object_offset);
    PDF_Out.Images.Clear_image_directory (pdf);
    pdf.is_closed := True;
  end Finish;

  ----------------------
  -- Output to a file --
  ----------------------

  procedure Create
    (pdf        : in out PDF_Out_File;
     file_name  : in     String;
     PDF_format : in     PDF_Type := default_PDF_type)
  is
  begin
    Reset (pdf, PDF_format);
    pdf.pdf_file := new Ada.Streams.Stream_IO.File_Type;
    Create (pdf.pdf_file.all, Out_File, file_name);
    pdf.file_name := To_Unbounded_String (file_name);
    pdf.pdf_stream := PDF_Raw_Stream_Class (Stream (pdf.pdf_file.all));
    Write_PDF_header (pdf);
  end Create;

  procedure Close (pdf : in out PDF_Out_File) is
    procedure Dispose is new
      Ada.Unchecked_Deallocation (Ada.Streams.Stream_IO.File_Type, PDF_File_Acc);
  begin
    Finish (PDF_Out_Stream (pdf));
    if pdf.file_name /= "nul" then  --  Test needed for OA 7.2.2 (Close raises Use_Error)
      Close (pdf.pdf_file.all);
    end if;
    Dispose (pdf.pdf_file);
  end Close;

  --  Set the index on the file
  overriding procedure Set_Index (pdf : in out PDF_Out_File;
                                  to  : in     Ada.Streams.Stream_IO.Positive_Count)
  is
  begin
    Ada.Streams.Stream_IO.Set_Index (pdf.pdf_file.all, to);
  end Set_Index;

  --  Return the index of the file
  overriding function Index (pdf : PDF_Out_File) return Ada.Streams.Stream_IO.Count
  is
  begin
    return Ada.Streams.Stream_IO.Index (pdf.pdf_file.all);
  end Index;

  function Is_Open (pdf : in PDF_Out_File) return Boolean is
  begin
    if pdf.pdf_file = null then
      return False;
    end if;
    return Ada.Streams.Stream_IO.Is_Open (pdf.pdf_file.all);
  end Is_Open;

  ------------------------
  -- Output to a string --
  ------------------------
  --  Code reused from Zip_Streams

  overriding procedure Read
    (Stream : in out Unbounded_Stream;
     Item   :    out Stream_Element_Array;
     Last   :    out Stream_Element_Offset)
  is
  begin
    --  Item is read from the stream. If (and only if) the stream is
    --  exhausted, Last will be < Item'Last. In that case, T'Read will
    --  raise an End_Error exception.
    --
    --  Cf: RM 13.13.1(8), RM 13.13.1(11), RM 13.13.2(37) and
    --  explanations by Tucker Taft
    --
    Last := Item'First - 1;
    --  if Item is empty, the following loop is skipped; if Stream.Loc
    --  is already indexing out of Stream.Unb, that value is also appropriate
    for i in Item'Range loop
      Item (i) := Character'Pos (Element (Stream.Unb, Stream.Loc));
      Stream.Loc := Stream.Loc + 1;
      Last := i;
    end loop;
  exception
    when Ada.Strings.Index_Error =>
      null; -- what could be read has been read; T'Read will raise End_Error
  end Read;

  overriding procedure Write
    (Stream : in out Unbounded_Stream;
     Item   : in     Stream_Element_Array)
  is
  begin
    for I in Item'Range loop
      if Length (Stream.Unb) < Stream.Loc then
        Append (Stream.Unb, Character'Val (Item (I)));
      else
        Replace_Element (Stream.Unb, Stream.Loc, Character'Val (Item (I)));
      end if;
      Stream.Loc := Stream.Loc + 1;
    end loop;
  end Write;

  procedure Set_Index (S : access Unbounded_Stream; To : Positive) is
  begin
    if Length (S.Unb) < To then
      for I in Length (S.Unb) .. To loop
        Append (S.Unb, ASCII.NUL);
      end loop;
    end if;
    S.Loc := To;
  end Set_Index;

  function Index (S : access Unbounded_Stream) return Integer is
  begin
    return S.Loc;
  end Index;

  --- ***

  procedure Create
    (pdf        : in out PDF_Out_String;
     PDF_format : in     PDF_Type := default_PDF_type)
  is
  begin
    Reset (pdf, PDF_format);
    pdf.pdf_memory := new Unbounded_Stream;
    pdf.pdf_memory.Unb := Null_Unbounded_String;
    pdf.pdf_memory.Loc := 1;
    pdf.pdf_stream := PDF_Raw_Stream_Class (pdf.pdf_memory);
    Write_PDF_header (pdf);
  end Create;

  procedure Close (pdf : in out PDF_Out_String) is
  begin
    Finish (PDF_Out_Stream (pdf));
  end Close;

  function Contents (pdf : PDF_Out_String) return String is
  begin
    if not pdf.is_closed then
      raise PDF_stream_not_closed;
    end if;
    return To_String (pdf.pdf_memory.Unb);
  end Contents;

  --  Set the index on the PDF string stream
  overriding procedure Set_Index (pdf : in out PDF_Out_String;
                                  to  : in     Ada.Streams.Stream_IO.Positive_Count)
  is
  begin
    Set_Index (pdf.pdf_memory, Integer (to));
  end Set_Index;

  --  Return the index of the PDF string stream
  overriding function Index (pdf : PDF_Out_String) return Ada.Streams.Stream_IO.Count
  is
  begin
    return Ada.Streams.Stream_IO.Count (Index (pdf.pdf_memory));
  end Index;

end PDF_Out;
