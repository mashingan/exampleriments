with Ada.Integer_Text_IO;
with Ada.Text_IO;

procedure forloops is
	package nio renames Ada.Integer_Text_IO;
	package tio renames Ada.Text_IO;
	procedure Sawatdee(Start, Stop: in Integer) is
	begin
		for i in Start..Stop loop
			nio.Put(i);
		end loop;
		tio.New_Line;
	end Sawatdee;

	type Week is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
	procedure loopweek is
	begin
		loop_name:
		for day in Week loop
			tio.Put_Line(Week'Image(day));
		end loop loop_name;
	end loopweek;

	Set: array(15 .. 60) of Integer := (others => 42);
	procedure Magandang_Umaga is
	begin
		for I in Set'range loop
			Set(I) := I;
		end loop;
		outer:
		for Index in Set'range loop
			tio.Put(Integer'image(index) & " ");
			tio.Put_Line(Integer'image(Set(index)));
			Inner:
			for Day in week loop
				tio.Put(Week'image(day) & " ");
			end loop Inner;
			tio.New_Line;
		end loop outer;
	end Magandang_Umaga;

	procedure Lakad_Matatag is
	begin
		for I in reverse Set'range loop
			Set(Set'last - I + Set'First) := I;
		end loop;
		for I in Set'range loop
			tio.Put_Line(I'image & " " & Set(I)'image);
		end loop;
	end Lakad_Matatag;
begin
	Sawatdee(1, 5);
	loopweek;
	tio.New_Line;
	Magandang_Umaga;
	Lakad_Matatag;
end forloops;
