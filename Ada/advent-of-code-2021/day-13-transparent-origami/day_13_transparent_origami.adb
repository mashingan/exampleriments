-- compile it: (small)
-- gnatmake -O2 Day_13_transparent_origami.adb -bargs -largs -s
-- (normal)
-- gnatmake Day_13_transparent_origami.adb
-- run in cmd (not powershell)
-- type input.txt | Day_13_transparent_origami.exe
-- to clean folder: gnatclean
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Containers.Vectors;

procedure Day_13_transparent_origami is
	package tio renames Ada.Text_IO;
	package nio renames Ada.Integer_Text_IO;
	package fix renames Ada.Strings.Fixed;

	type Point is record
		x: Natural;
		y: Natural;
	end record;

	function To_String(p: in Point) return String is
		('(' & p.x'image & ',' & p.y'image & ')');
	function "="(P1, P2: in Point) return Boolean is
		(P1.x = P2.x and P1.y = P2.y);
	function "<"(P1, P2: in Point) return Boolean is
		(P1.x < P2.x and P2.x < P2.y);

	package Points_Vector is new Ada.Containers.Vectors
		(Element_Type => Point, Index_Type => Natural);
	package pv renames Points_Vector;

	type Paper is record
		Height, Width: Natural := 0;
		Dots: pv.Vector := pv.Empty_Vector;
		Fold_x, Fold_y: Natural := 0;
	end record;

	Transparent_paper: Paper;
	fold_y_first: Boolean := false;
	is_fold_set: Boolean := false;

	procedure Include(v: in out pv.Vector; val: in Point) is
	begin
		if v.Contains(Val) then return; end if;
		v.Append(val);
	end Include;
	function Read_coord(line: in String; length: in Positive) return Boolean is
		index, x, y: Natural;
		last: Positive;
		p: Point;
	begin
		index := fix.Index(line, ",");
		if index <= 1 then return false; end if;
		nio.Get(line(line'first .. index-1), x, last);
		nio.Get(line(index+1 .. length), y, last);
		if x > Transparent_paper.Width then Transparent_paper.Width := x; end if;
		if y > Transparent_paper.Height then Transparent_paper.Height := y; end if;
		p := (x, y);
		tio.Put_Line("Read coord point: " & To_String(p));
		Include(Transparent_paper.Dots, (x, y));
		return true;
	end Read_coord;

	procedure Read_fold(line: in String; length: in Positive) is
		index: Natural;
		axis: Character;
		last: Positive;
	begin
		index := fix.Index(line, "=");
		if index < 13 then return; end if;
		axis := line(index-1);
		if axis = 'x' then
			nio.Get(line(index+1 .. length), Transparent_paper.Fold_x, last);
			if not is_fold_set then
				is_fold_set := true;
				fold_y_first := false;
			end if;
		elsif axis = 'y' then
			nio.Get(line(index+1 .. length), Transparent_paper.Fold_y, last);
			if not is_fold_set then
				is_fold_set := true;
				fold_y_first := true;
			end if;
		else null;
		end if;
	end Read_fold;

	procedure Fold_both(Fold_horizontal : in Boolean; Dim, Middle_fold: in out Natural) is
		newdots : pv.Vector := pv.Empty_Vector;
		fold_first_side : Boolean := true;
		procedure Map_dots(c: in pv.Cursor) is
			p: Point;
			newnum, prevnum : Natural;
		begin
			p := pv.Element(c);
			if Fold_horizontal then
				newnum := p.y;
				prevnum := p.y;
			else
				newnum := p.x;
				prevnum := p.x;
			end if;
			if fold_first_side then
				if prevnum >= Middle_fold then
					newnum := Middle_fold - abs(Middle_fold - prevnum);
				end if;
				if Fold_horizontal then
					p.y := newnum;
				else
					p.x := newnum;
				end if;
				Include(newdots, p);
			else
				if prevnum < Middle_fold then
					newnum := abs(Middle_fold - prevnum);
				else
					newnum := prevnum - Middle_fold;
				end if;
				if Fold_horizontal then
					p.y := newnum;
				else
					p.x := newnum;
				end if;
				Include(newdots, p);
			end if;
		end Map_dots;
	begin
		if Middle_fold < (Dim / 2) then
			fold_first_side := false;
		else
			Dim := Middle_fold;
		end if;
		Transparent_paper.Dots.Iterate(Map_dots'access);
		Transparent_paper.Dots := newdots;
	end Fold_both;

	procedure Read_Manual is
		line: string(1..20);
		length: Natural;
	begin
		loop
			begin
				tio.Get_Line(line, length);
			exception
				when tio.Data_Error => exit;
				when tio.End_Error => exit;
			end;
			tio.Put_Line("Read line: " & line(line'first .. length) & " with length" & length'image);
			if length > 0 then
				if not Read_coord(line, length) then
					Read_fold(line, length);
				end if;
			end if;
		end loop;
	end Read_Manual;

	procedure Inspect_Paper is
		paper_dim: array(0 .. Transparent_paper.Width, 0 .. Transparent_paper.Height) of Character;

		procedure Mark_dot(c: in pv.Cursor) is
			p: Point;
		begin
			p := pv.Element(c);
			tio.Put_Line(To_String(p));
			if p.x > Transparent_paper.Width or p.y > Transparent_paper.Height then return; end if;
			if p.x < 0 or p.y < 0 then return; end if;
			paper_dim(p.x, p.y) := '#';
		end Mark_dot;
	begin
		for I in 0 .. Transparent_paper.Width loop
			for J in 0 .. Transparent_paper.Height loop
				paper_dim(I, J) := '.';
			end loop;
		end loop;
		tio.Put("Paper width:" & Transparent_paper.Width'image);
		tio.Put(", height:" & Transparent_paper.Height'image);
		tio.New_Line;
		tio.Put("Paper dots read:" & Integer(Transparent_paper.Dots.Length)'image);
		tio.Put(", with fold x:" & Transparent_paper.Fold_x'image);
		tio.Put(" and fold y:" & Transparent_paper.Fold_y'image);
		tio.New_Line;
		Transparent_paper.Dots.Iterate(Mark_dot'access);
		for H in 0 .. Transparent_paper.Height loop
			for W in 0 .. Transparent_paper.Width loop
				tio.Put(paper_dim(W, H));
			end loop;
			tio.New_Line;
		end loop;
	end Inspect_Paper;

begin
	Read_Manual;
	Inspect_Paper;
	if fold_y_first then
		--Fold_y;
		Fold_both(true, Transparent_paper.Height, Transparent_paper.Fold_y);
	else
		Fold_both(false, Transparent_paper.Width, Transparent_paper.Fold_x);
		--Fold_x;
	end if;
	tio.New_Line;
	tio.Put_Line("After first fold");
	tio.New_Line;
	Inspect_Paper;
	tio.Put_Line("Visible dot after first fold:" & Integer(Transparent_paper.Dots.Length)'image);
end Day_13_transparent_origami;
