-- compile it:
-- gnatmake -O2 Day_11_dumbo_octopus.adb -bargs -largs -s
-- run in cmd (not powershell)
-- type input.txt | Day_11_dumbo_octopus.exe
-- to clean folder: adaclean
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Day_11_dumbo_octopus is
	package tio renames Ada.Text_IO;
	package nio renames Ada.Integer_Text_IO;

	subtype Energy is Integer range 0 .. 9;
	type Grid_Width is range 1 .. 10;

	type Octopus is record
		self_energy: Energy;
		flashed: Boolean := false;
	end record;

	type Grid is array(Grid_Width, Grid_Width) of Octopus;

	Octo_grid: Grid;

	procedure Draw_Grid is
		s: String(1..1);
	begin
		for I in Grid_Width loop
			for J in Grid_Width loop
				nio.Put(s, Octo_grid(I, J).self_energy);
				tio.Put(s);
			end loop;
			tio.New_Line;
		end loop;
	end Draw_Grid;

	procedure Read_Grid is
		linebuf: String(Integer(Grid_Width'first) .. Integer(Grid_Width'last) + 1);
		length: Natural;
		last: Positive;
		e: Energy;
	begin
		for I in Grid_Width loop
			begin
				tio.Get_Line(linebuf, length);
			exception
				when tio.Data_Error => exit;
				when tio.End_Error => exit;
			end;
			--tio.Put_Line("Read line:" & linebuf & " with length" & length'image);
			for J in linebuf'first .. length loop
				nio.Get(linebuf(J) & "", e, last);
				Octo_grid(I, Grid_Width(J)) := (e, false);
			end loop;
		end loop;
	end Read_Grid;

	procedure Energy_Propagation(g: in out Grid; I, J: in Integer)
	is
		Iw, Jw: Grid_Width;
	begin
		if I < Integer(Grid_Width'first) or I > Integer(Grid_Width'last) or
			J < Integer(Grid_Width'first) or J > Integer(Grid_Width'last) then
			return;
		end if;
		Iw := Grid_Width(I);
		Jw := Grid_Width(J);
		if g(Iw, Jw).flashed then return; end if;
		if g(Iw, Jw).self_energy + 1 > Energy'last then
			g(Iw, Jw) := (0, true);
			for Ik in -1 .. 1 loop
				for Jk in -1 .. 1 loop
					--tio.Put_Line("I+Ik:" & Integer'image(I+Ik) & ", J+JK:" & Integer'image(J+Jk));
					Energy_Propagation(g, I + Ik, J + Jk);
				end loop;
			end loop;
		else
			g(Iw, Jw).self_energy := g(Iw, Jw).self_energy + 1;
		end if;
	end Energy_Propagation;

	function Clean_Flashes(g: in out Grid) return Integer is
		flashes: Integer := 0;
	begin
		for I in Grid_Width loop
			for J in Grid_Width loop
				if g(I, J).flashed then
					flashes := flashes + 1;
					g(I, J).flashed := false;
				end if;
			end loop;
		end loop;
		return flashes;
	end Clean_Flashes;

	function Flash_step(g: in out Grid) return Integer is
	begin
		for I in Grid_Width loop
			for J in Grid_Width loop
				Energy_Propagation(Octo_grid, Integer(I), Integer(J));
			end loop;
		end loop;
		return Clean_Flashes(Octo_grid);
	end Flash_step;

	function Flashes_steps(Target: in Integer) return Integer is
		flashes : Integer := 0;
	begin
		for N in 1 .. Target loop
			flashes := flashes + Flash_step(Octo_grid);
		end loop;
		return flashes;
	end Flashes_steps;

	flashed : Integer := 0;
	Target_steps: constant := 100;

begin
	Read_Grid;
	flashed := Flashes_steps(Target_steps);
	tio.Put_Line("Total flashed:" & flashed'image);
end Day_11_dumbo_octopus;
