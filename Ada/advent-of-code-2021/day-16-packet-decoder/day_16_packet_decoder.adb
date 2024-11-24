-- compile it: (small)
-- gnatmake -O2 Day_16_packet_decoder.adb -bargs -largs -s
-- (normal)
-- gnatmake Day_16_packet_decoder.adb
-- run in cmd (not powershell)
-- type input.txt | Day_16_packet_decoder.exe
-- or simply run:
-- build-run.bat
-- to clean folder: gnatclean
with Ada.Text_IO;
with Packet_Decoders;

procedure Day_16_packet_decoder is
	package tio renames Ada.Text_IO;
	package ps renames Packet_Decoders;

	sample_packet : string := "D2FE28";
	num : Integer := 0;

	procedure Read_Packets is
		line: String(1..40);
		length: Integer;
	begin
		loop
			begin
				tio.Get_Line(line, length);
			exception
				when tio.Data_Error => exit;
				when tio.End_Error => exit;
			end;
			exit when length = 0;
			tio.Put_Line("sum of version from packet " & line(1..length) &
			" is" & ps.Decode_Packet(line(1..length))'image);
		end loop;
	end Read_Packets;
begin
	Read_Packets;
end Day_16_packet_decoder;
