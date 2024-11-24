-- compile it:
-- gnatmake -O2 Day_03_Binary_Diagnostic.adb -bargs -largs -s
-- run in cmd (not powershell)
-- type input.txt | Day_03_Binary_Diagnostic.exe
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

procedure Day_03_Binary_Diagnostic is
	package tio renames Ada.Text_IO;
	package nio renames Ada.Integer_Text_IO;
	package fix renames Ada.Strings.Fixed;
	line: string(1..5) := fix."*"(5, ' ');
	empty: string(1..5) := fix."*"(5, ' ');
	last: Natural;
	type Diagnostic is array(1..5) of Integer;
	diagnostic_values: Diagnostic := (others => 0);
	gamma_rate, epsilon_rate: Integer := 0;

	procedure feed_diagnostic(diag: in string; diagval: in out Diagnostic) is
		displacement: Integer;
	begin
		--tio.Put_Line("the line:" & line);
		for i in diag'range loop
			displacement := if diag(i) = '1' then 1 else -1;
			--tio.Put_Line("displacement:" & displacement'image);
			diagval(i) := diagval(i) + displacement;
		end loop;
	end feed_diagnostic;
begin
	loop
		begin
			tio.Get_Line(line, last);
			--tio.Put_Line("Line:" & line & "Last:" & last'image);
		exception
			when tio.Data_Error => exit;
			when tio.End_Error => exit;
		end;
		exit when line = empty;
		feed_diagnostic(line, diagnostic_values);
	end loop;
	--tio.Put_Line("final diagnostic values:" & diagnostic_values'image);
	for i in diagnostic_values'range loop
		declare
			twomult: Integer;
		begin
			twomult := 2 ** (diagnostic_values'last - i);
			--tio.Put_Line("i:" & i'image & ", twomult:" & twomult'image);
			if diagnostic_values(i) > 0 then
				gamma_rate := gamma_rate + twomult;
			else
				epsilon_rate := epsilon_rate + twomult;
			end if;
		end;
	end loop;
	--tio.Put_Line("gamma:" & gamma_rate'image & ", epsilon:" & epsilon_rate'image);
	tio.Put_Line("final value is" & Integer'image(gamma_rate * epsilon_rate));
end Day_03_Binary_Diagnostic;
