-------------------------------------------------------------------------------------
--
-- PDF_OUT - A low level package for writing Adobe Acrobat PDF (*) files
--
-- Pure Ada 95 code, 100% portable: OS-, CPU- and compiler- independent.
--
-- Version / date / download info: see the version, reference, web strings
--   defined at the end of the public part of this package.

-- Legal licensing note:

--  Copyright (c) 2014 .. 2015 Gautier de Montmollin

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
--
--------------------------------------------------------------------------

with Ada.Calendar;                      use Ada.Calendar;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;

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
  PDF_stream_not_closed : exception;

  type PDF_type is (
    PDF_1_3 -- PDF 1.3
  );

  Default_PDF_type: constant PDF_type:= PDF_1_3;

  subtype Real is Long_Float;

  ----------------------------
  -- (2) Document contents: --
  ----------------------------

  procedure Put(pdf: in out PDF_Out_Stream; num : Real);
  procedure Put(pdf    : in out PDF_Out_Stream;
                num   : in Integer;
                width : in Ada.Text_IO.Field := 0; -- ignored
                base  : in Ada.Text_IO.Number_Base := 10
            );
  procedure Put(pdf: in out PDF_Out_Stream; str : String);
  procedure Put(pdf: in out PDF_Out_Stream; str : Unbounded_String);
  procedure Put(pdf: in out PDF_Out_Stream; date: Time);
  --
  procedure Put_Line(pdf: in out PDF_Out_Stream; num : Real);
  procedure Put_Line(pdf: in out PDF_Out_Stream; num : Integer);
  procedure Put_Line(pdf: in out PDF_Out_Stream; str : String);
  procedure Put_Line(pdf: in out PDF_Out_Stream; str : Unbounded_String);
  procedure Put_Line(pdf: in out PDF_Out_Stream; date: Time);
  --
  procedure New_Line(pdf: in out PDF_Out_Stream; Spacing : Positive := 1);
  procedure New_Page(pdf: in out PDF_Out_Stream);
  --
  procedure Text_XY(pdf: in out PDF_Out_Stream; x,y: Long_Float);

  function Col(pdf: in PDF_Out_Stream) return Positive;
  function Line(pdf: in PDF_Out_Stream) return Positive;
  function Page(pdf: in PDF_Out_Stream) return Natural;

  type Color_Type is record
    red, green, blue: Real;
  end record;

  black: constant Color_Type:= (0.0,0.0,0.0);

  procedure Color(pdf: in out PDF_Out_Stream; c: Color_Type);
  procedure Stroking_Color(pdf: in out PDF_Out_Stream; c: Color_Type);

  --  You need to override the Header and Footer methods
  --  for setting up your custom header and footer. By default they do nothing.
  procedure Page_Header(pdf : in out PDF_Out_Stream);
  procedure Page_Footer(pdf : in out PDF_Out_Stream);

  --
  --  Page layout procedures.
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

  one_cm: constant:= 72.0 / 2.54;
  cm_2_5: constant:= one_cm * 2.5;

  cm_2_5_margins: constant Margins_Type:= (cm_2_5, cm_2_5, cm_2_5, cm_2_5);

  procedure Margins(pdf : out PDF_Out_Stream; new_margins: Margins_Type);
  function Margins(pdf : PDF_Out_Stream) return Margins_Type;

  type Rectangle is record
    x_min, y_min,
    x_max, y_max : Real;
  end record;

  --  A4 is 21.0 x 29.7 cm
  A4_portrait : constant Rectangle:= (0.0, 0.0, 21.0 * one_cm, 29.7 * one_cm);
  A4_landscape: constant Rectangle:= (0.0, 0.0, A4_portrait.y_max, A4_portrait.x_max);

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

  version   : constant String:= "DRAFT"; -- 001 Preview 1
  reference : constant String:= "xx-yyy-2015";
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

  type Offset_table is array(1..1000) of Ada.Streams.Stream_IO.Count;
  -- !! size hardcoded

  type Page_table is array(1..1000) of Positive; -- object ID's of pages
  -- !! size hardcoded

  -- Some unique objects like Pages need to have a pre-determined index,
  -- otherwise single Page objects don't know their parent's index.
  pages_idx: constant:= 1;
  last_fix_obj_idx: constant:= 1;

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
  end record;

  type PDF_Out_Stream is abstract new PDF_Out_Pre_Root_Type with null record;

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
