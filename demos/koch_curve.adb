--  http://www.rosettacode.org/wiki/Koch_curve#Ada
--  Author: Jesper Quorning

with Ada.Numerics.Generic_Elementary_Functions;
with PDF_Out;

procedure Koch_Curve is

   package Real_Math is
     new Ada.Numerics.Generic_Elementary_Functions (PDF_Out.Real);
   use Real_Math, PDF_Out;

   subtype Angle_Deg  is Real;
   type    Level_Type is range 0 .. 7;

   Purple : constant Color_Type := (0.35, 0.0, 0.25);
   Length : constant Real       := 400.0;
   Corner : constant Point      := (90.0, 580.0);

   procedure Draw_Image (Level : Level_Type) is

   Current   : Point      := (0.0, 0.0);
   Direction : Angle_Deg  := Angle_Deg'(60.0);
   Doc       : PDF_Out_File;

   procedure Koch (Level : Level_Type; Length : Real) is
   begin
      if Level = 0 then
         Current := Current + Length * Point'(Sin (Direction, 360.0),
                                              Cos (Direction, 360.0));
         Doc.Line (Corner + Current);
      else
         Koch (Level - 1, Length / 3.0);  Direction := Direction -  60.0;
         Koch (Level - 1, Length / 3.0);  Direction := Direction + 120.0;
         Koch (Level - 1, Length / 3.0);  Direction := Direction -  60.0;
         Koch (Level - 1, Length / 3.0);
      end if;
   end Koch;

   begin
      Doc.Create ("koch_" & Character'Val (Character'Pos ('0') + Level) & ".pdf");
      Doc.Page_Setup (A4_portrait);
      Doc.Margins (Margins_Type'(left   => cm_2_5,
                                 others => one_cm));
      Doc.Color (Purple);
      Doc.Move (Corner);
      for Count in 1 .. 3 loop
         Koch (Level, Length);
         Direction := Direction + 120.0;
      end loop;
      Doc.Finish_Path (close_path => True,
                       rendering  => fill,
                       rule       => even_odd);
      Doc.Close;
   end Draw_Image;

begin
   for Level in Level_Type loop
      Draw_Image (Level);
   end loop;
end Koch_Curve;
