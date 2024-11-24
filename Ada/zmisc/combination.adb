-- compile:
-- gnatmake -gnat2022 combination.adb
with Ada.Text_IO;

procedure Combination is
   package tio renames Ada.Text_IO;
   type Id is (A, B, C);
   type Pos is array (1..3) of Id;

   procedure comb(posin: in Pos; posout: in out Pos; startidx, endidx, idx, r: in Integer) is
      count : Integer := startidx;
   begin
      --tio.Put_Line ("startidx:" & startidx'image);
      --tio.Put_Line ("endidx:" & endidx'image);
      --tio.Put_Line ("idx:" & idx'image);
      if idx > r then
         for I in 1..3 loop
            tio.Put(posout(I)'image);
         end loop;
         --posout := (C, C, C);
         tio.New_Line;
         return;
      end if;
      for I in startidx .. endidx loop
         exit when endidx-I+1 < r-idx;
         posout(idx) := posin(I);
         --tio.Put_Line("posout(" & idx'image & ") :=" & posout(idx)'image);
         --tio.Put_Line("posin(" & I'image & ") :=" & posin(I)'image);
         comb(posin, posout, i+1, endidx, idx+1, r);
      end loop;
   end comb;

   procedure comb2(posin: in Pos) is
   begin
      for I in 1..3 loop
         for J in 1..3 loop
            for K in 1..3 loop
               if (i /= j and j /= k and k /= i) then
                  tio.Put(posin(I)'image & ',' & posin(J)'image & ',' & posin(K)'image);
                  tio.New_Line;
               end if;
            end loop;
         end loop;
      end loop;
   end comb2;

   input : Pos := (A, B, C);
   output : Pos;
begin
   tio.Put_Line("start combination of " & input'image);
   comb(input, output, Pos'first, Pos'last, Pos'first, Pos'last);
   comb2 (input);

end Combination;
