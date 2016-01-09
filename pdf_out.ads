-------------------------------------------------------------------------------------
--
-- PDF_OUT - A low level package for writing Adobe Acrobat PDF (*) files
--
-- Pure Ada 95 code, 100% portable: OS-, CPU- and compiler- independent.
--
-- Version / date / download info: see the version, reference, web strings
--   defined at the end of the public part of this package.

-- Legal licensing note:

--  Copyright (c) 2014 .. 2016 Gautier de Montmollin

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

-- NB: this is the MIT License, as found 12-Sep-2007 on the site
-- http://www.opensource.org/licenses/mit-license.php

-- (*) All Trademarks mentioned are properties of their respective owners.
-------------------------------------------------------------------------------------
--
--  Follow these steps to create an PDF document stream:
--
--  1. Create
--
--  2. | Write(pdf, row, column, data):
--     | Put(pdf, data)               :
--     | New_Line(pdf),...            : other "Text_IO"-like (full list below)
--
--  3. Close
--
--  4. (PDF_Out_String only) function Contents returns the full .pdf
--
--  Header and footer are set up by overriding the corresponding methods.
--
--  Note: the standard PDF measurement unit is a "point", set as 1/72 inch.
--  All technical references are to PDF 1.7 format, ISO 32000-1:2008 standard
--  http://www.adobe.com/devnet/pdf/pdf_reference.html
--
--------------------------------------------------------------------------

with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;

with System;

package PDF_Out is

  -----------------------------------------------------------------
  -- The abstract PDF output stream root type.                 --
  -- From this package, you can use the following derived types: --
  --    * PDF_Out_File    : output in a file                   --
  --    * PDF_Out_String  : output in a string                 --
  -- Of course you can define your own derived types.            --
  -----------------------------------------------------------------

  type PDF_Out_Stream is abstract tagged private;

  PDF_stream_not_created,
  PDF_stream_not_closed,
  Not_implemented: exception;

  type PDF_type is (
    PDF_1_3 -- PDF 1.3
  );

  Default_PDF_type: constant PDF_type:= PDF_1_3;

  type Real is digits System.Max_Digits;
  package Real_IO is new Ada.Text_IO.Float_IO(Real);

  type Point is record
    x, y : Real;
  end record;

  function "+"(P1,P2: Point) return Point;
  pragma Inline("+");

  function "*"(f: Real; P: Point) return Point;
  pragma Inline("*");

  type Rectangle is record
    x_min, y_min,
    width, height : Real;
  end record;

  function X_Max(r: Rectangle) return Real;
  function Y_Max(r: Rectangle) return Real;

  ----------------------------
  -- (2) Document contents: --
  ----------------------------

  procedure Put(pdf  : in out PDF_Out_Stream;
                num  : in Real;
                fore : in Ada.Text_IO.Field := Real_IO.Default_Fore;
                aft  : in Ada.Text_IO.Field := Real_IO.Default_Aft;
                exp  : in Ada.Text_IO.Field := Real_IO.Default_Exp
            );
  procedure Put(pdf   : in out PDF_Out_Stream;
                num   : in Integer;
                width : in Ada.Text_IO.Field := 0; -- ignored
                base  : in Ada.Text_IO.Number_Base := 10
            );
  procedure Put(pdf: in out PDF_Out_Stream; str : String);
  procedure Put(pdf: in out PDF_Out_Stream; str : Unbounded_String);
  --
  procedure Put_Line(pdf  : in out PDF_Out_Stream;
                     num  : in Real;
                     fore : in Ada.Text_IO.Field := Real_IO.Default_Fore;
                     aft  : in Ada.Text_IO.Field := Real_IO.Default_Aft;
                     exp  : in Ada.Text_IO.Field := Real_IO.Default_Exp
            );
  procedure Put_Line(pdf   : in out PDF_Out_Stream;
                     num   : in Integer;
                     width : in Ada.Text_IO.Field := 0; -- ignored
                     base  : in Ada.Text_IO.Number_Base := 10
            );
  procedure Put_Line(pdf: in out PDF_Out_Stream; str : String);
  procedure Put_Line(pdf: in out PDF_Out_Stream; str : Unbounded_String);
  --
  procedure New_Line(pdf: in out PDF_Out_Stream; Spacing : Positive := 1);
  procedure New_Page(pdf: in out PDF_Out_Stream);
  --  Call to Finish_Page is optional, but can be necessary in some circumstances,
  --  for instance for displaying the footer correctly before changing page
  --  orientation or margins for the following pages.
  procedure Finish_Page(pdf: in out PDF_Out_Stream);
  --
  procedure Text_XY(pdf: in out PDF_Out_Stream; x,y: Real);
  procedure Put_XY(pdf: in out PDF_Out_Stream; x,y: Real; str : String);

  function Col(pdf: in PDF_Out_Stream) return Positive;
  function Line(pdf: in PDF_Out_Stream) return Positive;
  function Page(pdf: in PDF_Out_Stream) return Natural;

  type Font_Type is
     (--  The 14 standard fonts
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
      External_Font
     );

  subtype Standard_Font_Type is Font_Type range Courier .. Zapf_Dingbats;

  procedure Font(pdf: in out PDF_Out_Stream; f: Standard_Font_Type);
  procedure Font_Size(pdf: in out PDF_Out_Stream; size: Real);
  procedure Line_Spacing(pdf: in out PDF_Out_Stream; factor: Real);  --  as multiple of font size
  default_line_spacing: constant:= 1.2;
  procedure Line_Spacing_Pt(pdf: in out PDF_Out_Stream; pt: Real);   --  in Point units

  type Color_Type is record
    red, green, blue: Real;
  end record;

  black: constant Color_Type:= (0.0,0.0,0.0);

  procedure Color(pdf: in out PDF_Out_Stream; c: Color_Type);
  procedure Stroking_Color(pdf: in out PDF_Out_Stream; c: Color_Type);

  type Rendering_Mode is (
    fill, stroke, fill_then_stroke, invisible,
    --  Same, but also add text to path for clipping.
    fill_and_add_to_path,
    stroke_and_add_to_path,
    fill_then_stroke_and_add_to_path,
    add_to_path
  );

  procedure Text_Rendering_Mode(pdf: in out PDF_Out_Stream; r: Rendering_Mode);

  ---------------
  --  Graphics --
  ---------------

  procedure Image(pdf: in out PDF_Out_Stream; file_name: String; target: Rectangle);

  -----------------------
  --  Vector graphics  --
  -----------------------

  initial_line_width: constant:= 1.0; --  See Table 52, 8.4.1
  procedure Line_Width(pdf: in out PDF_Out_Stream; width: Real);

  --  Draw a single line segment:
  procedure Single_Line(pdf: in out PDF_Out_Stream; from, to: Point);

  subtype Path_Rendering_Mode is Rendering_Mode range fill .. fill_then_stroke;

  --  Draw a single rectangle:
  procedure Draw(pdf: in out PDF_Out_Stream; what: Rectangle; rendering: Path_Rendering_Mode);

  --  Paths:

  type Inside_path_rule is (nonzero_winding_number, even_odd);
  --  Rule to determine how to fill areas within a (non-trivial) path.
  --  See 8.5.3.3.2 and 8.5.3.3.3 of PDF specification

  procedure Move(pdf: in out PDF_Out_Stream; to: Point);
  procedure Line(pdf: in out PDF_Out_Stream; to: Point);
  procedure Cubic_Bezier(pdf: in out PDF_Out_Stream; control_1, control_2: Point; to: Point);
  --  All lines and curves and the eventual filling inside the path
  --  will be drawn when path is completed, with Finish_Path:

  procedure Finish_Path(
    pdf        : in out PDF_Out_Stream;
    close_path :        Boolean;
    rendering  :        Path_Rendering_Mode;  --  fill, stroke, or both
    rule       :        Inside_path_rule
  );

  -----------
  --  Misc --
  -----------

  --  If some PDF feature is not yet implemented in this package,
  --  you can insert direct PDF code - at your own risk ;-).
  procedure Insert_PDF_Code(pdf: in out PDF_Out_Stream; code: String);
  --
  --  Image functions for numbers, designed to take the least place
  --  possible without loss of precision (useful for inserting PDF code).
  function Img(p: Integer) return String;
  function Img( x: Real; prec: Positive:= Real'Digits ) return String;

  --  Document information
  procedure Title(pdf: in out PDF_Out_Stream; s: String);
  procedure Author(pdf: in out PDF_Out_Stream; s: String);
  procedure Subject(pdf: in out PDF_Out_Stream; s: String);
  procedure Keywords(pdf: in out PDF_Out_Stream; s: String);
  procedure Creator_Application(pdf: in out PDF_Out_Stream; s: String);

  ------------------
  --  Page layout --
  ------------------

  --  You need to override the Header and Footer methods
  --  for setting up your custom header and footer. By default they do nothing.
  procedure Page_Header(pdf : in out PDF_Out_Stream);
  procedure Page_Footer(pdf : in out PDF_Out_Stream);

  --  They have to be called before New_Page in order to influence the next page.
  --  For the first page, call them before any output (typically right after Create).
  --
  procedure Left_Margin(pdf : out PDF_Out_Stream; pts: Real);
  function Left_Margin(pdf : PDF_Out_Stream) return Real;
  procedure Right_Margin(pdf : out PDF_Out_Stream; pts: Real);
  function Right_Margin(pdf : PDF_Out_Stream) return Real;
  procedure Top_Margin(pdf : out PDF_Out_Stream; pts: Real);
  function Top_Margin(pdf : PDF_Out_Stream) return Real;
  procedure Bottom_Margin(pdf : out PDF_Out_Stream; pts: Real);
  function Bottom_Margin(pdf : PDF_Out_Stream) return Real;
  --
  type Margins_Type is record
    left, right, top, bottom: Real;
  end record;

  --  Some distances in Points

  one_cm   : constant:= 72.0 / 2.54;
  cm_2_5   : constant:= one_cm * 2.5;
  one_inch : constant:= 72.0;

  cm_2_5_margins: constant Margins_Type:= (cm_2_5, cm_2_5, cm_2_5, cm_2_5);

  procedure Margins(pdf : out PDF_Out_Stream; new_margins: Margins_Type);
  function Margins(pdf : PDF_Out_Stream) return Margins_Type;

  --  A4 is 21.0 x 29.7 cm
  A4_portrait : constant Rectangle:= (0.0, 0.0, 21.0 * one_cm, 29.7 * one_cm);
  A4_landscape: constant Rectangle:= (0.0, 0.0, A4_portrait.height, A4_portrait.width);

  procedure Page_Setup(pdf : in out PDF_Out_Stream; layout: Rectangle);

  function Layout(pdf : PDF_Out_Stream) return Rectangle;

  -----------------------------------------------------------------
  -- Here, the derived stream types pre-defined in this package. --
  -----------------------------------------------------------------
  -- * Output to a file:

  type PDF_Out_File is new PDF_Out_Stream with private;

  procedure Create(
    pdf        : in out PDF_Out_File;
    file_name  :        String;
    PDF_format :        PDF_type:= Default_PDF_type
  );

  procedure Close(pdf : in out PDF_Out_File);

  function Is_Open(pdf : in PDF_Out_File) return Boolean;

  -- * Output to a string (to be compressed, packaged, transmitted, ... ):

  type PDF_Out_String is new PDF_Out_Stream with private;

  procedure Create(
    pdf        : in out PDF_Out_String;
    PDF_format :        PDF_type:= Default_PDF_type
  );

  procedure Close(pdf : in out PDF_Out_String);

  function Contents(pdf: PDF_Out_String) return String;

  --------------------------------------------------------------
  -- Information about this package - e.g. for an "about" box --
  --------------------------------------------------------------

  version   : constant String:= "001, preview 1";
  reference : constant String:= "9-Jan-2016";
  web       : constant String:= "http://apdf.sf.net/";
  -- hopefully the latest version is at that URL...  ---^

  ----------------------------------
  -- End of the part for the user --
  ----------------------------------

  -- Set the index on the stream
  procedure Set_Index (pdf: in out PDF_Out_Stream;
                       to: Ada.Streams.Stream_IO.Positive_Count)
  is abstract;

  -- Return the index of the stream
  function Index (pdf: PDF_Out_Stream) return Ada.Streams.Stream_IO.Count
  is abstract;

private

  type Page_zone is (nowhere, in_page, in_header, in_footer);

  type Offset_table is array(1..100_000) of Ada.Streams.Stream_IO.Count;
  -- !! size hardcoded

  type Page_table is array(1..10_000) of Positive; -- object ID's of pages
  -- !! size hardcoded

  -- Some unique objects like Pages need to have a pre-determined index,
  -- otherwise single Page objects don't know their parent's index.
  pages_idx: constant:= 1;
  last_fix_obj_idx: constant:= 1;

  type Dir_node;
  type p_Dir_node is access Dir_node;

  type Dir_node(name_len: Natural) is record
    left, right      : p_Dir_node;
    file_name        : String(1..name_len);
    image_index      : Positive;
    pdf_object_index : Natural:= 0;  --  0 = not yet insterted into the PDF stream
    local_resource   : Boolean;      --  All True items to be listed into Resource dictionary
  end record;

  ----------------------------------------
  -- Raw Streams, with 'Read and 'Write --
  ----------------------------------------

  type PDF_Raw_Stream_Class is access all Ada.Streams.Root_Stream_Type'Class;

  -- We have a concrete type as hidden ancestor of the PDF_Out_Stream root
  -- type. A variable of that type is initialized with default values and
  -- can help re-initialize a PDF_Out_Stream when re-used several times.
  -- See the Reset procedure in body.
  -- The abstract PDF_Out_Stream could have default values, but using a
  -- variable of this type to reset values is not Ada compliant (LRM:3.9.3(8))
  --
  type PDF_Out_Pre_Root_Type is tagged record
    pdf_stream    : PDF_Raw_Stream_Class;
    start_index   : Ada.Streams.Stream_IO.Count;
    is_created    : Boolean     := False;
    is_closed     : Boolean     := False;
    format        : PDF_type    := Default_PDF_type;
    zone          : Page_zone   := nowhere;
    last_page     : Natural     := 0;
    current_line  : Positive    := 1;  --  Mostly for Ada.Text_IO compatibility
    current_col   : Positive    := 1;  --  Mostly for Ada.Text_IO compatibility
    page_idx      : Page_table;
    page_box      : Rectangle   := A4_portrait;
    maximum_box   : Rectangle   := A4_portrait;
    page_margins  : Margins_Type:= cm_2_5_margins;
    objects       : Natural     := last_fix_obj_idx;
    object_offset : Offset_table;
    stream_obj_buf: Unbounded_String;
    img_dir_tree  : p_Dir_node  := null;
    img_count     : Natural     := 0;
    current_font  : Font_Type   := Helvetica;
    font_size     : Real        := 11.0;
    line_spacing  : Real        := default_line_spacing;
    ext_font_name : Unbounded_String;
    doc_title     : Unbounded_String;  --  Document information (14.3.3)
    doc_author    : Unbounded_String;  --  Document information (14.3.3)
    doc_subject   : Unbounded_String;  --  Document information (14.3.3)
    doc_keywords  : Unbounded_String;  --  Document information (14.3.3)
    doc_creator   : Unbounded_String;  --  Document information (14.3.3) : creator application
  end record;

  type PDF_Out_Stream is abstract new PDF_Out_Pre_Root_Type with null record;

  --  For child packages
  function Image_name(i: Positive) return String;
  procedure New_object(pdf : in out PDF_Out_Stream'Class);
  procedure WL(pdf : in out PDF_Out_Stream'Class; s: String);
  procedure Copy_file(
    file_name  : String;
    into       : in out Ada.Streams.Root_Stream_Type'Class;
    buffer_size: Positive:= 1024*1024
  );

  ----------------------
  -- Output to a file --
  ----------------------

  type PDF_file_acc is
    access Ada.Streams.Stream_IO.File_Type;

  type PDF_Out_File is new PDF_Out_Stream with record
    pdf_file   : PDF_file_acc:= null; -- access to the "physical" PDF file
  end record;

  -- Set the index on the file
  procedure Set_Index (pdf: in out PDF_Out_File;
                       To: Ada.Streams.Stream_IO.Positive_Count);

  -- Return the index of the file
  function Index (pdf: PDF_Out_File) return Ada.Streams.Stream_IO.Count;

  ------------------------
  -- Output to a string --
  ------------------------
  -- Code reused from Zip_Streams

  --- *** We define here a complete in-memory stream:
  type Unbounded_Stream is new Ada.Streams.Root_Stream_Type with
    record
      Unb : Ada.Strings.Unbounded.Unbounded_String;
      Loc : Integer := 1;
    end record;

  -- Read data from the stream.
  procedure Read
    (Stream : in out Unbounded_Stream;
     Item   : out Ada.Streams.Stream_Element_Array;
     Last   : out Ada.Streams.Stream_Element_Offset);

  -- write data to the stream, starting from the current index.
  -- Data will be overwritten from index is already available.
  procedure Write
    (Stream : in out Unbounded_Stream;
     Item   : Ada.Streams.Stream_Element_Array);

  -- Set the index on the stream
  procedure Set_Index (S : access Unbounded_Stream; To : Positive);

  -- returns the index of the stream
  function Index (S: access Unbounded_Stream) return Integer;

  --- ***

  type Unbounded_Stream_Acc is access Unbounded_Stream;

  type PDF_Out_String is new PDF_Out_Stream with record
    pdf_memory: Unbounded_Stream_Acc;
  end record;

  -- Set the index on the PDF string stream
  procedure Set_Index (pdf: in out PDF_Out_String;
                       To: Ada.Streams.Stream_IO.Positive_Count);

  -- Return the index of the PDF string stream
  function Index (pdf: PDF_Out_String) return Ada.Streams.Stream_IO.Count;

end PDF_Out;
