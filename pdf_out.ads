-------------------------------------------------------------------------------------
--
--  PDF_OUT - A low level package for writing Adobe Acrobat PDF (*) files
--
--  Pure Ada 2012+ code, 100% portable: OS-, CPU- and compiler- independent.
--
--  Version / date / download info: see the version, reference, web strings
--    defined at the end of the public part of this package.

--  Legal licensing note:

--  Copyright (c) 2014 .. 2025 Gautier de Montmollin

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

--  NB: this is the MIT License, as found 12-Sep-2007 on the site
--  http://www.opensource.org/licenses/mit-license.php

--  (*) All Trademarks mentioned are properties of their respective owners.
-------------------------------------------------------------------------------------
--
--  Follow the steps below to create a PDF document stream.
--  You can also get inspiration from the demos, tests & tools.
--
--  1. Create
--
--  2. | Put (pdf, data),
--     | New_Line (pdf), ... : other "Text_IO"-like (full list below)
--     | Image (pdf, ...)    : raster images
--     | Move/Line/...       : vector graphics
--     | New_Page (pdf)
--
--  3. Close
--
--  4. (PDF_Out_String only) function Contents returns the full .pdf
--
--  Header and footer are set up by overriding the corresponding methods.
--
--  Note: the standard PDF measurement unit is a "point", set as 1/72 inch.
--
--  All technical references are to PDF 1.7 format, ISO 32000-1:2008 standard
--  https://opensource.adobe.com/dc-acrobat-sdk-docs/standards/pdfstandards/pdf/PDF32000_2008.pdf
--
--------------------------------------------------------------------------

with Ada.Numerics.Generic_Elementary_Functions,
     Ada.Streams.Stream_IO,
     Ada.Strings.Unbounded,
     Ada.Strings.Wide_Unbounded,
     Ada.Strings.Wide_Wide_Unbounded,
     Ada.Text_IO;

with System;

package PDF_Out is

  -------------------------------------------------------------------
  --  The abstract PDF output stream root type.                    --
  --  From this package, you can use the following derived types:  --
  --     * PDF_Out_File    : output in a file                      --
  --     * PDF_Out_String  : output in a string                    --
  --  Of course you can define your own derived types.             --
  -------------------------------------------------------------------

  type PDF_Out_Stream is abstract tagged private;

  PDF_stream_not_created,
  PDF_stream_not_closed,
  Not_implemented : exception;

  type PDF_Type is
    (PDF_1_3);  --  PDF 1.3

  default_PDF_type : constant PDF_Type := PDF_1_3;

  type Real is digits System.Max_Digits;

  package Real_IO is new Ada.Text_IO.Float_IO (Real);
  package Real_Elementary_Functions is
    new Ada.Numerics.Generic_Elementary_Functions (Real);

  type Point is record
    x, y : Real;
  end record;

  function "+"(P1, P2 : Point) return Point;
  pragma Inline ("+");

  function "*"(f : Real; P : Point) return Point;
  pragma Inline ("*");

  function L1_Distance (P1, P2 : Point) return Real;
  pragma Inline (L1_Distance);

  --  Euclidean distance:
  function L2_Distance_Squared (P1, P2 : Point) return Real;
  pragma Inline (L2_Distance_Squared);

  type Rectangle is record
    x_min, y_min,
    width, height : Real;
  end record;

  function "+"(P : Point; r : Rectangle) return Rectangle;
  pragma Inline ("+");

  --  Scaling. r.x_min and r.y_min are preserved.
  function "*"(f : Real; r : Rectangle) return Rectangle;
  pragma Inline ("*");

  function X_Max (r : Rectangle) return Real;
  function Y_Max (r : Rectangle) return Real;

  function Expand (r : Rectangle; left, right, up, down : Real) return Rectangle;

  use Ada.Strings.Unbounded;

  ------------------------------
  --  (2) Document contents:  --
  ------------------------------

  procedure Put (pdf  : in out PDF_Out_Stream;
                 num  : in Real;
                 fore : in Ada.Text_IO.Field := Real_IO.Default_Fore;
                 aft  : in Ada.Text_IO.Field := Real_IO.Default_Aft;
                 exp  : in Ada.Text_IO.Field := Real_IO.Default_Exp);

  procedure Put (pdf   : in out PDF_Out_Stream;
                 num   : in Integer;
                 width : in Ada.Text_IO.Field       := 0;  --  ignored
                 base  : in Ada.Text_IO.Number_Base := 10);

  procedure Put (pdf : in out PDF_Out_Stream; str : String);
  procedure Put (pdf : in out PDF_Out_Stream; str : Unbounded_String);

  procedure Put_Line (pdf  : in out PDF_Out_Stream;
                      num  : in Real;
                      fore : in Ada.Text_IO.Field := Real_IO.Default_Fore;
                      aft  : in Ada.Text_IO.Field := Real_IO.Default_Aft;
                      exp  : in Ada.Text_IO.Field := Real_IO.Default_Exp);

  procedure Put_Line (pdf   : in out PDF_Out_Stream;
                      num   : in Integer;
                      width : in Ada.Text_IO.Field       := 0;  --  ignored
                      base  : in Ada.Text_IO.Number_Base := 10);

  procedure Put_Line (pdf : in out PDF_Out_Stream; str : String);
  procedure Put_Line (pdf : in out PDF_Out_Stream; str : Unbounded_String);

  procedure New_Line (pdf : in out PDF_Out_Stream; Spacing : Positive := 1);
  procedure New_Page (pdf : in out PDF_Out_Stream);

  --  Support for Wide_String's, usually containing UTF-16 or UCS-2 strings.
  --  Caution: only the ISO-8859-1 (Latin-1) subset is currently supported.

  procedure Put_WS (pdf : in out PDF_Out_Stream; w_str : Wide_String);

  procedure Put_Line_WS
     (pdf : in out PDF_Out_Stream; w_str : Wide_String);

  procedure Put_Line_WS
     (pdf   : in out PDF_Out_Stream;
      w_str :        Ada.Strings.Wide_Unbounded.Unbounded_Wide_String);

  --  Support for Wide_Wide_String's, containing UTF-32 strings.
  --  Caution: only the ISO-8859-1 (Latin-1) subset is currently supported.

  procedure Put_WWS (pdf : in out PDF_Out_Stream; ww_str : Wide_Wide_String);

  procedure Put_Line_WWS
     (pdf : in out PDF_Out_Stream; ww_str : Wide_Wide_String);

  procedure Put_Line_WWS
     (pdf    : in out PDF_Out_Stream;
      ww_str :        Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String);

  --  Call to Finish_Page is optional, but can be necessary in some circumstances,
  --  for instance for displaying the footer correctly before changing page
  --  orientation or margins for the following pages.
  procedure Finish_Page (pdf : in out PDF_Out_Stream);

  procedure Text_XY (pdf : in out PDF_Out_Stream; x, y : Real);
  procedure Put_XY (pdf : in out PDF_Out_Stream; x, y : Real; str : String);
  procedure Put_XY (pdf : in out PDF_Out_Stream; pt : Point; str : String);

  function Col (pdf : in PDF_Out_Stream) return Positive;
  function Line (pdf : in PDF_Out_Stream) return Positive;
  function Page (pdf : in PDF_Out_Stream) return Natural;

  type Font_Type is
     ( --  The 14 standard fonts
      Courier,
      Courier_Bold,
      Courier_Bold_Oblique,
      Courier_Oblique,
      Helvetica,
      Helvetica_Bold,
      Helvetica_Bold_Oblique,
      Helvetica_Oblique,
      Symbol,
      Times_Bold,
      Times_Bold_Italic,
      Times_Italic,
      Times_Roman,
      Zapf_Dingbats,
      --  Fonts imported into the PDF document
      External_Font);

  subtype Standard_Font_Type is Font_Type range Courier .. Zapf_Dingbats;

  --  Select one of the Adobe PDF standard fonts.
  --  The encoding is on 8 bits and follows the "Windows Code Page 1252"
  --  encoding (called WinAnsiEncoding in the PDF standard).
  --  See Annex D, especially "Table D.1 - Latin-text encodings" for details.
  procedure Font (pdf : in out PDF_Out_Stream; f : Standard_Font_Type);

  --  Set the font size.
  --  In general the size is a scale factor (see Table 105, Tf operator).
  --  For standard fonts the unit seems to be the Point (pt).
  procedure Font_Size (pdf : in out PDF_Out_Stream; size : Real);
  function Get_Font_Size (pdf : PDF_Out_Stream) return Real;

  --  Get the bounding box of a text with current font settings.
  --  The origin (0, 0) is the point where the text is meant to be displayed
  --  with Put_XY (pdf, 0.0, 0.0, text) - see the arrow in the diagram below.
  --  Then, Draw (pdf, Bounding_Box (pdf, text)) will frame the text displayed
  --  by Put_XY. See the Hyperlink method for an example.
  --
  --               +-------------- ...
  --               |     __
  --               |    /  \
  --               |    |  |
  --       _______\|    \__|
  --              /|       |
  --               |       |
  --               +-------------- ...
  --
  function Bounding_Box (pdf : PDF_Out_Stream; text : String) return Rectangle;

  procedure Line_Spacing (pdf : in out PDF_Out_Stream; factor : Real);  --  as multiple of font size
  default_line_spacing : constant := 1.2;
  procedure Line_Spacing_Pt (pdf : in out PDF_Out_Stream; pt : Real);   --  in Point (pt) units

  --------------
  --  Colors  --
  --------------

  --  0.0 = minimum intensity
  --  1.0 = maximum intensity.
  subtype Color_Value is Real range 0.0 .. 1.0;

  type Color_Type is record
    red, green, blue : Color_Value;
  end record;

  black : constant Color_Type := (0.0, 0.0, 0.0);

  procedure Color (pdf : in out PDF_Out_Stream; c : Color_Type);
  procedure Filling_Color (pdf : in out PDF_Out_Stream; c : Color_Type) renames Color;
  procedure Stroking_Color (pdf : in out PDF_Out_Stream; c : Color_Type);

  type Rendering_Mode is
    (fill, stroke, fill_then_stroke, invisible,
     --  Same, but also add text to path for clipping.
     fill_and_add_to_path,
     stroke_and_add_to_path,
     fill_then_stroke_and_add_to_path,
     add_to_path);

  procedure Text_Rendering_Mode (pdf : in out PDF_Out_Stream; r : Rendering_Mode);

  --  Ubiquitous 8-bit-per-channel Red-Green-Blue code.
  type RGB_Code_Range is range 0 .. 16#FF_FF_FF#;

  function Convert (rgb_code : RGB_Code_Range) return Color_Type;

  ----------------
  --  Graphics  --
  ----------------

  --  Insert an image from a file
  procedure Image (pdf : in out PDF_Out_Stream; file_name : String; target : Rectangle);

  --  For calibrating the target rectangle in the Image procedure, you may need this:
  function Get_Pixel_Dimensions (image_file_name : String) return Rectangle;

  --  Caution: scaling is up to you! The rectangle returned by the function
  --  is (0.0, 0.0, width, height), with 1 pixel = 1pt.

  -----------------------
  --  Vector graphics  --
  -----------------------

  initial_line_width : constant := 1.0;  --  See Table 52, 8.4.1
  procedure Line_Width (pdf : in out PDF_Out_Stream; width : Real);

  --  Draw a single line segment:
  procedure Single_Line (pdf : in out PDF_Out_Stream; from, to : Point);

  subtype Path_Rendering_Mode is Rendering_Mode range fill .. fill_then_stroke;

  --  Draw simple figures.
  --  Rectangle:
  procedure Draw
    (pdf       : in out PDF_Out_Stream;
     what      :        Rectangle;
     rendering :        Path_Rendering_Mode);

  --  Paths:

  type Inside_path_rule is (nonzero_winding_number, even_odd);
  --  Rule to determine how to fill areas within a (non-trivial) path.
  --  See 8.5.3.3.2 and 8.5.3.3.3 of PDF specification

  procedure Move (pdf : in out PDF_Out_Stream; to : Point);
  procedure Line (pdf : in out PDF_Out_Stream; to : Point);

  procedure Cubic_Bezier
    (pdf                  : in out PDF_Out_Stream;
     control_1, control_2 : in     Point;
     to                   : in     Point);

  --  Arc of a circle.
  --  Parameters as in PostScript Language Reference, 8.2.
  --  Angles are in degrees, counterclockwise from the (1,0) point.
  --
  procedure Arc
    (pdf              : in out PDF_Out_Stream;
     center           : in     Point;
     radius           : in     Real;
     angle_1, angle_2 : in     Real;
     line_to_start    : in     Boolean);

  procedure Circle
    (pdf       : in out PDF_Out_Stream;
     center    : in     Point;
     radius    : in     Real;
     rendering : in     Path_Rendering_Mode);

  --  All lines and curves and the possible filling inside the path
  --  will be drawn when path is completed, with Finish_Path:
  --
  procedure Finish_Path
    (pdf        : in out PDF_Out_Stream;
     close_path :        Boolean;
     rendering  :        Path_Rendering_Mode;  --  fill, stroke, or both
     rule       :        Inside_path_rule);

  -------------------
  --  Annotations  --
  -------------------

  --  Link to an Internet / Intranet resource outside the document.
  procedure Hyperlink
    (pdf     : in out PDF_Out_Stream;
     area    : in     Rectangle;
     visible : in     Boolean;
     url     : in     String);

  procedure Hyperlink
    (pdf     : in out PDF_Out_Stream;
     origin  : in     Point;
     text    : in     String;
     visible : in     Boolean;
     url     : in     String);

  unspecified_position : constant := -1;

  --  Link to a page within the document.
  --  NB: you need to produce the same PDF document twice
  --  in order to have a correct hyperlink.
  --
  procedure Hyperlink
    (pdf     : in out PDF_Out_Stream;
     area    : in     Rectangle;
     visible : in     Boolean;
     page    : in     Positive;
     y_pos   : in     Integer := unspecified_position);

  procedure Hyperlink
    (pdf     : in out PDF_Out_Stream;
     origin  : in     Point;
     text    : in     String;
     visible : in     Boolean;
     page    : in     Positive;
     y_pos   : in     Integer := unspecified_position);

  ---------------------
  --  Miscellaneous  --
  ---------------------

  --  In the likely case some PDF feature is not yet implemented in
  --  this package, you can insert direct PDF code - at your own risk ;-).
  --
  --  NB: the state the PDF machine is either in text-writing
  --  mode, or graphics mode. To make outputs compliant with the PDF
  --  standard, if you want to insert graphics code, please
  --  use the Insert_Graphics_PDF_Code below. For text-related stuff,
  --  use Insert_Text_PDF_Code.
  --
  procedure Insert_PDF_Code (pdf : in out PDF_Out_Stream; code : String);
  pragma Inline (Insert_PDF_Code);

  --  This is for direct text PDF code insertion (text-writing mode
  --  will be switched on). In PDF language these are the T... commands.
  --
  procedure Insert_Text_PDF_Code (pdf : in out PDF_Out_Stream; code : String);

  --  This is for direct graphics PDF code insertion (text-writing mode
  --  will be switched off for the graphics output).
  --
  procedure Insert_Graphics_PDF_Code (pdf : in out PDF_Out_Stream; code : String);

  max_displayed_digits : constant Positive := Integer'Min (10, Real'Digits);

  --  Image (representation in digits) functions for numbers, designed to
  --  take the least possible room, albeit without significant loss of precision.
  --  Useful for inserting PDF code.
  function Img (p : Integer) return String;
  function Img (x : Real; prec : Positive := max_displayed_digits) return String;

  --  Document information
  procedure Title (pdf : in out PDF_Out_Stream; s : String);
  procedure Author (pdf : in out PDF_Out_Stream; s : String);
  procedure Subject (pdf : in out PDF_Out_Stream; s : String);
  procedure Keywords (pdf : in out PDF_Out_Stream; s : String);
  procedure Creator_Application (pdf : in out PDF_Out_Stream; s : String);

  -------------------
  --  Page layout  --
  -------------------

  --  You need to override the Header and Footer methods
  --  for setting up your custom header and footer. By default they do nothing.
  procedure Page_Header (pdf : in out PDF_Out_Stream);
  procedure Page_Footer (pdf : in out PDF_Out_Stream);

  --  They have to be called before New_Page in order to influence the next page.
  --  For the first page, call them before any output (typically right after Create).
  --
  procedure Left_Margin (pdf : out PDF_Out_Stream; pts : Real);
  function Left_Margin (pdf : PDF_Out_Stream) return Real;
  procedure Right_Margin (pdf : out PDF_Out_Stream; pts : Real);
  function Right_Margin (pdf : PDF_Out_Stream) return Real;
  procedure Top_Margin (pdf : out PDF_Out_Stream; pts : Real);
  function Top_Margin (pdf : PDF_Out_Stream) return Real;
  procedure Bottom_Margin (pdf : out PDF_Out_Stream; pts : Real);
  function Bottom_Margin (pdf : PDF_Out_Stream) return Real;
  --
  type Margins_Type is record
    left, right, top, bottom : Real;
  end record;

  --  Some distances in Points

  one_cm   : constant := 72.0 / 2.54;
  cm_2_5   : constant := one_cm * 2.5;
  one_inch : constant := 72.0;

  cm_2_5_margins : constant Margins_Type := (cm_2_5, cm_2_5, cm_2_5, cm_2_5);

  procedure Margins (pdf : out PDF_Out_Stream; new_margins : Margins_Type);
  function Margins (pdf : PDF_Out_Stream) return Margins_Type;

  --  A4 is 21.0 x 29.7 cm
  A4_portrait : constant Rectangle := (0.0, 0.0, 21.0 * one_cm, 29.7 * one_cm);
  A4_landscape : constant Rectangle := (0.0, 0.0, A4_portrait.height, A4_portrait.width);

  procedure Page_Setup (pdf : in out PDF_Out_Stream; layout : Rectangle);

  function Layout (pdf : PDF_Out_Stream) return Rectangle;

  --  Set_Index and Index are not directly useful for PDF_Out users.
  --  They are private indeed, but they must be visible (RM 3.9.3(10)).

  --  Set the index on the stream
  procedure Set_Index (pdf : in out PDF_Out_Stream;
                       to : Ada.Streams.Stream_IO.Positive_Count)
  is abstract;

  --  Return the index of the stream
  function Index (pdf : PDF_Out_Stream) return Ada.Streams.Stream_IO.Count
  is abstract;

  -------------------------------------------------------------------
  --  Here are derived stream types, pre-defined in this package.  --
  -------------------------------------------------------------------
  --  * Output to a file:

  type PDF_Out_File is new PDF_Out_Stream with private;

  procedure Create
    (pdf        : in out PDF_Out_File;
     file_name  : in     String;
     PDF_format : in     PDF_Type := default_PDF_type);

  procedure Close (pdf : in out PDF_Out_File);

  function Is_Open (pdf : in PDF_Out_File) return Boolean;

  --  * Output to a string (to be compressed, packaged, transmitted, ... ):

  type PDF_Out_String is new PDF_Out_Stream with private;

  procedure Create
    (pdf        : in out PDF_Out_String;
     PDF_format : in     PDF_Type := default_PDF_type);

  procedure Close (pdf : in out PDF_Out_String);

  function Contents (pdf : PDF_Out_String) return String;

  ----------------------------------------------------------------
  --  Information about this package - e.g. for an "about" box  --
  ----------------------------------------------------------------

  version   : constant String := "008";
  reference : constant String := "22-Mar-2025";
  --  Hopefully the latest version is at one of those URLs:
  web  : constant String := "https://apdf.sourceforge.io/";
  web2 : constant String := "https://sourceforge.net/projects/apdf/";
  web3 : constant String := "https://github.com/zertovitch/ada-pdf-writer";
  web4 : constant String := "https://alire.ada.dev/crates/apdf";

private

  min_bits : constant := Integer'Max (32, System.Word_Size);
  --  13.3(8): A word is the largest amount of storage that can be
  --  conveniently and efficiently manipulated by the hardware,
  --  given the implementation's run-time model.

  type PDF_Index_Type is range -2**(min_bits - 1) .. 2**(min_bits - 1) - 1;
  --  We define an Integer type which is at least 32 bits, but n bits
  --  on a native n > 32 bits architecture (no performance hit on 64+
  --  bits architectures).

  type Offset_Table is array (PDF_Index_Type range <>) of Ada.Streams.Stream_IO.Count;
  type p_Offset_Table is access Offset_Table;

  type Page_Table is array (PDF_Index_Type range <>) of PDF_Index_Type;  --  Object ID's of pages
  type p_Page_Table is access Page_Table;

  --  Some unique objects like Pages need to have a pre-determined index,
  --  otherwise single Page objects don't know their parent's index.
  pages_idx : constant PDF_Index_Type := 1;
  last_fix_obj_idx : constant PDF_Index_Type := 1;

  type Dir_Node;
  type p_Dir_Node is access Dir_Node;

  type Dir_node (name_len : Natural) is record
    left, right      : p_Dir_Node;
    image_index      : Positive;
    pdf_object_index : PDF_Index_Type := 0;  --  0 = not yet insterted into the PDF stream
    local_resource   : Boolean;              --  Items to be listed into a page's Resource dictionary
    file_name        : String (1 .. name_len);
  end record;

  type Page_Zone is (nowhere, in_page, in_header, in_footer);
  type Text_or_Graphics is (text, graphics);

  type Standard_Font_Set is array (Standard_Font_Type) of Boolean;

  ------------------------------------------
  --  Raw Streams, with 'Read and 'Write  --
  ------------------------------------------

  type PDF_Raw_Stream_Class is access all Ada.Streams.Root_Stream_Type'Class;

  --  We have a concrete type as hidden ancestor of the PDF_Out_Stream root
  --  type. A variable of that type is initialized with default values and
  --  can help re-initialize a PDF_Out_Stream when re-used several times.
  --  See the Reset procedure in body.
  --  The abstract PDF_Out_Stream could have default values, but using a
  --  variable of this type to reset values is not Ada compliant (LRM:3.9.3(8))
  --
  type PDF_Out_Pre_Root_Type is tagged record
    pdf_stream            : PDF_Raw_Stream_Class;
    start_index           : Ada.Streams.Stream_IO.Count;
    is_created            : Boolean           := False;
    is_closed             : Boolean           := False;
    format                : PDF_Type          := default_PDF_type;
    zone                  : Page_Zone         := nowhere;
    text_switch           : Text_or_Graphics  := graphics;
    last_page             : PDF_Index_Type    := 0;
    current_line          : Positive          := 1;  --  Mostly for Ada.Text_IO compatibility
    current_col           : Positive          := 1;  --  Mostly for Ada.Text_IO compatibility
    page_idx              : p_Page_Table      := null;  --  page_idx(p): Object ID of page p
    old_page_idx          : p_Page_Table      := null;  --  Needed for internal Hyperlink.
    page_box              : Rectangle         := A4_portrait;
    maximum_box           : Rectangle         := A4_portrait;
    page_margins          : Margins_Type      := cm_2_5_margins;
    objects               : PDF_Index_Type    := last_fix_obj_idx;
    object_offset         : p_Offset_Table    := null;
    stream_obj_buf        : Unbounded_String;
    img_dir_tree          : p_Dir_Node        := null;
    img_count             : Natural           := 0;
    current_font          : Font_Type         := Helvetica;
    font_size             : Real              := 11.0;
    line_spacing          : Real              := default_line_spacing;
    ext_font_name         : Unbounded_String;
    current_annot         : Unbounded_String;
    std_font_used_in_page : Standard_Font_Set := (others => False);
    doc_title             : Unbounded_String;  --  Document information (14.3.3)
    doc_author            : Unbounded_String;  --  Document information (14.3.3)
    doc_subject           : Unbounded_String;  --  Document information (14.3.3)
    doc_keywords          : Unbounded_String;  --  Document information (14.3.3)
    doc_creator           : Unbounded_String;  --  Document information (14.3.3) : creator application
  end record;

  type PDF_Out_Stream is abstract new PDF_Out_Pre_Root_Type with null record;

  --  For child packages
  function Image_Name (i : Positive) return String;
  procedure New_Object (pdf : in out PDF_Out_Stream'Class);
  procedure WL (pdf : in out PDF_Out_Stream'Class; s : String);
  pragma Inline (WL);

  procedure Copy_File
    (file_name   :        String;
     into        : in out Ada.Streams.Root_Stream_Type'Class;
     buffer_size :        Positive := 1024 * 1024);

  ------------------------
  --  Output to a file  --
  ------------------------

  type PDF_File_Acc is
    access Ada.Streams.Stream_IO.File_Type;

  type PDF_Out_File is new PDF_Out_Stream with record
    pdf_file   : PDF_File_Acc := null; -- access to the "physical" PDF file
    file_name  : Unbounded_String;
  end record;

  --  Set the index on the file
  overriding procedure Set_Index (pdf : in out PDF_Out_File;
                                  to  : in     Ada.Streams.Stream_IO.Positive_Count);

  --  Return the index of the file
  overriding function Index (pdf : PDF_Out_File) return Ada.Streams.Stream_IO.Count;

  --------------------------
  --  Output to a string  --
  --------------------------
  --  Code reused from Zip_Streams

  --- *** We define here a complete in-memory stream:
  type Unbounded_Stream is new Ada.Streams.Root_Stream_Type with
    record
      Unb : Ada.Strings.Unbounded.Unbounded_String;
      Loc : Integer := 1;
    end record;

  --  Read data from the stream.
  overriding procedure Read
    (Stream : in out Unbounded_Stream;
     Item   :    out Ada.Streams.Stream_Element_Array;
     Last   :    out Ada.Streams.Stream_Element_Offset);

  --  write data to the stream, starting from the current index.
  --  Data will be overwritten from index is already available.
  overriding procedure Write
    (Stream : in out Unbounded_Stream;
     Item   : Ada.Streams.Stream_Element_Array);

  --  Set the index on the stream
  procedure Set_Index (S : access Unbounded_Stream; To : Positive);

  --  returns the index of the stream
  function Index (S : access Unbounded_Stream) return Integer;

  --- ***

  type Unbounded_Stream_Acc is access Unbounded_Stream;

  type PDF_Out_String is new PDF_Out_Stream with record
    pdf_memory : Unbounded_Stream_Acc;
  end record;

  --  Set the index on the PDF string stream
  overriding procedure Set_Index (pdf : in out PDF_Out_String;
                                  to  : in     Ada.Streams.Stream_IO.Positive_Count);

  --  Return the index of the PDF string stream
  overriding function Index (pdf : PDF_Out_String) return Ada.Streams.Stream_IO.Count;

end PDF_Out;
