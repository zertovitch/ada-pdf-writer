--  http://www.rosettacode.org/wiki/Pinstripe/Printer#Ada
--  Author: Jesper Quorning

with Ada.Text_IO;

with PDF_Out;

procedure Pinstripe_Printer
is
   use PDF_Out;

   package Real_IO
   is new Ada.Text_Io.Float_IO (Real);

   procedure Pinstripe (Doc          : in out PDF_Out_File;
                        Line_Width   : Real;
                        Line_Height  : Real;
                        Screen_Width : Real;
                        Y            : Real)
   is
      Count  : constant Natural
        := Natural (Real'Floor (Screen_Width / (2.0 * Line_Width)));
      Corner      : constant Point := (Doc.Left_Margin, Doc.Bottom_Margin);
      Corner_Box  : constant Point := Corner     + (10.0, 10.0);
      Corner_Text : constant Point := Corner_Box + (10.0, 10.0);
      Light_Gray  : constant Color_Type := (0.9, 0.9, 0.9);
      Image : String (1 .. 4);
   begin
      --  Pinstripes
      Doc.Color (black);
      for A in 0 .. Count loop
         Doc.Draw (what => Corner +
                     Rectangle'(x_min  => 2.0 * Real (A) * Line_Width,
                                y_min  => Y,
                                width  => Line_Width,
                                height => Line_Height),
                   rendering => fill);
      end loop;

      --  Box
      Doc.Stroking_Color (black);
      Doc.Color (Light_Gray);
      Doc.Line_Width (3.0);
      Doc.Draw (what => Corner_Box + (0.0, Y, 120.0, 26.0),
                rendering => fill_then_stroke);
      --  Text
      Doc.Color (black);
      Doc.Text_Rendering_Mode (fill);
      Real_IO.Put (Image, Line_Width, Aft => 1, Exp => 0);
      Doc.Put_XY (Corner_Text.x, Corner_Text.y + Y,
                  Image & " point pinstripe");
   end Pinstripe;

   Doc : PDF_Out_File;
begin
   Doc.Create ("pinstripe.pdf");
   Doc.Page_Setup (A4_portrait);
   Doc.Margins (Margins_Type'(left   => cm_2_5,
                              others => one_cm));
   declare
      Width  : constant Real
        := A4_portrait.width - Doc.Left_Margin - Doc.Right_Margin;
      Height : constant Real
        := A4_portrait.height - Doc.Top_Margin - Doc.Bottom_Margin;
   begin
      for Point in 1 .. 11 loop
         Pinstripe (Doc,
                    Line_Width   => Real (Point),
                    Line_Height  => one_inch,
                    Screen_Width => Width,
                    Y            => Height - Real (Point) * one_inch);
      end loop;
   end;
   Doc.Close;
end Pinstripe_Printer;
