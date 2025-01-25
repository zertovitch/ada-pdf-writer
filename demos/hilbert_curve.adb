--  https://rosettacode.org/wiki/Hilbert_curve#Ada
--  Author: Jesper Quorning

with PDF_Out;

procedure Hilbert_Curve is

   use PDF_Out;

   Length  : constant := 500.0;
   Corner  : constant Point := (50.0, 300.0);

   type Rule_Type is (A, B, C, D);

   PDF   : PDF_Out.PDF_Out_File;
   First : Boolean;

   procedure Hilbert (Order  : in Natural;
                      Rule   : in Rule_Type;
                      Length : in Real;
                      X, Y   : in Real)
   is
      L : constant Real := Length / 4.0;
   begin
      if Order = 0 then
         if First then
            First := False;
            PDF.Move (Corner + (X, Y));
         else
            PDF.Line (Corner + (X, Y));
         end if;
      else
         case Rule is
            when A =>
               Hilbert (Order - 1, D, 2.0 * L, X - L, Y + L);
               Hilbert (Order - 1, A, 2.0 * L, X - L, Y - L);
               Hilbert (Order - 1, A, 2.0 * L, X + L, Y - L);
               Hilbert (Order - 1, B, 2.0 * L, X + L, Y + L);
            when B =>
               Hilbert (Order - 1, C, 2.0 * L, X + L, Y - L);
               Hilbert (Order - 1, B, 2.0 * L, X - L, Y - L);
               Hilbert (Order - 1, B, 2.0 * L, X - L, Y + L);
               Hilbert (Order - 1, A, 2.0 * L, X + L, Y + L);
            when C =>
               Hilbert (Order - 1, B, 2.0 * L, X + L, Y - L);
               Hilbert (Order - 1, C, 2.0 * L, X + L, Y + L);
               Hilbert (Order - 1, C, 2.0 * L, X - L, Y + L);
               Hilbert (Order - 1, D, 2.0 * L, X - L, Y - L);
            when D =>
               Hilbert (Order - 1, A, 2.0 * L, X - L, Y + L);
               Hilbert (Order - 1, D, 2.0 * L, X + L, Y + L);
               Hilbert (Order - 1, D, 2.0 * L, X + L, Y - L);
               Hilbert (Order - 1, C, 2.0 * L, X - L, Y - L);
         end case;
      end if;
   end Hilbert;

   procedure Hilbert (Order : Natural; Color : Color_Type) is
   begin
      First := True;
      PDF.Stroking_Color (Color);
      Hilbert (Order, A, Length, Length / 2.0, Length / 2.0);
      PDF.Finish_Path (close_path => False,
                       rendering  => stroke,
                       rule       => nonzero_winding_number);
   end Hilbert;

begin
   PDF.Create ("hilbert-curve.pdf");
   PDF.Page_Setup (A4_portrait);

   for depth in reverse 1 .. 7 loop
      PDF.Line_Width (if depth = 7 then 1.0 else 2.0);
      PDF.Color (black);
      PDF.Draw (Corner + (0.0, 0.0, Length, Length), fill);

      Hilbert (depth,     Color => (0.9, 0.1, 0.8));
      Hilbert (depth - 1, Color => (0.0, 0.9, 0.0));

      if depth > 1 then
         PDF.New_Page;
      end if;
   end loop;

   PDF.Close;
end Hilbert_Curve;
