--  http://www.rosettacode.org/wiki/Sierpinski_arrowhead_curve#Ada
--  Author: Jesper Quorning

with Ada.Numerics.Generic_Elementary_Functions;
with PDF_Out;

procedure Sierpinski_Arrowhead_Curve is

   package Real_Math is
     new Ada.Numerics.Generic_Elementary_Functions (PDF_Out.Real);
   use Real_Math, PDF_Out;

   subtype Angle_Deg  is Real;
   type    Order_Type is range 0 .. 7;

   Purple : constant Color_Type := (0.35, 0.0, 0.25);
   Length : constant Real       := 340.0;
   Corner : constant Point      := (120.0, 480.0);

   procedure Draw_Image (Order : Order_Type) is

      Current   : Point      := (0.0, 0.0);
      Direction : Angle_Deg  := Angle_Deg'(0.0);
      Doc       : PDF_Out_File;

      procedure Curve (Order : Order_Type; Length : Real; Angle : Angle_Deg) is
      begin
         if Order = 0 then
            Current := Current + Length * Point'(Cos (Direction, 360.0),
                                                 Sin (Direction, 360.0));
            Doc.Line (Corner + Current);
         else
            Curve (Order - 1, Length / 2.0, -Angle);  Direction := Direction + Angle;
            Curve (Order - 1, Length / 2.0,  Angle);  Direction := Direction + Angle;
            Curve (Order - 1, Length / 2.0, -Angle);
         end if;
      end Curve;

   begin
      Doc.Create ("sierp_" & Character'Val (Character'Pos ('0') + Order) & ".pdf");
      Doc.Page_Setup (A4_portrait);
      Doc.Margins (Margins_Type'(left   => cm_2_5,
                                 others => one_cm));
      Doc.Stroking_Color (Purple);
      Doc.Line_Width (2.0);
      Doc.Move (Corner);
      if Order mod 2 = 0 then
         Direction := 0.0;
         Curve (Order, Length, 60.0);
      else
         Direction := 60.0;
         Curve (Order, Length, -60.0);
      end if;
      Doc.Finish_Path (close_path => False,
                       rendering  => stroke,
                       rule       => nonzero_winding_number);
      Doc.Close;
   end Draw_Image;

begin
   for Order in Order_Type loop
     Draw_Image (Order);
   end loop;
end Sierpinski_Arrowhead_Curve;
