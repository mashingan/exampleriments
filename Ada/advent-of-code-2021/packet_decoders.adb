package body Packet_Decoders is
	function To_Hexadecimal(c: Character) return Hexadecimal is
		hx : Hexadecimal;
		last : Positive;
	begin
		eio.Get(c'image, hx, last);
		return hx;
	end To_Hexadecimal;

	function To_Integer(h: Hexadecimal) return Integer is (Hexadecimal'pos(h));

	function To_Bits(i: Integer) return bv.Vector is
		num : Integer := i;
		res : bv.Vector := bv.Empty_Vector;
	begin
		res.Reserve_Capacity(4);
		loop
			exit when num = 0;
			if num rem 2 = 0 then
				res.Append(false);
			else
				res.Append(true);
			end if;
			num := num / 2;
		end loop;
		for I in Integer(res.Length) .. 3 loop
			res.Append(false);
		end loop;
		res.Reverse_Elements;
		return res;
	end To_Bits;

	function To_Bits(h: in Hexadecimal) return bv.Vector is
		(To_Bits(To_Integer(h)));

	function To_Bits(c: in Character) return bv.Vector is
		(To_Bits(To_Integer(To_Hexadecimal(c))));

	function To_Integer(b: in bv.Vector) return Integer is
		num : Integer := 0;
		mul : Integer := 0;
		val : Integer := 0;
	begin
		for I in reverse b.First_Index .. b.Last_Index loop
			mul := 2 ** I;
			if b(b.Last_Index - I) then
				val := 1;
			else
				val := 0;
			end if;
			num := num + (val * mul);
		end loop;
		return num;
	end To_Integer;

	procedure Check_bits(b: in bv.Vector) is
	begin
		for I in b.Iterate loop
			if b(I) then
				tio.Put('1');
			else
				tio.Put('0');
			end if;
		end loop;
		tio.New_Line;
	end Check_bits;

	function Packet_Bits(s: string) return bv.Vector is
		vec : bv.Vector := bv.Empty_Vector;
	begin
		--tio.Put_Line("Packet: " & s);
		for I in s'range loop
			for C in To_bits(s(I)).Iterate loop
				vec.Append(bv.Element(C));
			end loop;
		end loop;
		return vec;
	end Packet_Bits;

	function Read_Version_Type(C: in out bv.Cursor) return Integer is
		buf : bv.Vector;
	begin
		buf.Append(false);
		for I in 1 .. 3 loop
			buf.Append(bv.Element(C));
			bv.Next(C);
		end loop;
		--Check_bits(buf);
		return To_Integer(buf);
	end Read_Version_Type;

	function Decode_Literal_bits(C: in out bv.Cursor; buf : in out bv.Vector) return Boolean is
		last_packet : Boolean := bv.Element(C);
	begin
		bv.Next(C);
		for I in 1 .. 4 loop
			if bv."="(C, bv.No_Element) then exit; end if;
			buf.Append(bv.Element(C));
			bv.Next(C);
		end loop;
		return last_packet;
	end Decode_Literal_bits;

	function Decode_Literal(C: in out bv.Cursor) return Integer is
		buf : bv.Vector;
		total : Integer := 0;
	begin
		loop
			exit when bv."="(C, bv.No_Element);
			if not Decode_Literal_bits(C, buf) then exit; end if;
		end loop;
		--return To_Integer(buf);
		return 0;
	end Decode_Literal;

	function Decode_Operator_0(C: in out bv.Cursor) return Integer is
		buflen : bv.Vector := bv.Empty_Vector;
		bitlen : Integer := 0;
		count : Integer := 0;
		curpos : Integer := 0;
		newpos : Integer := 0;
		total : Integer := 0;
	begin
		for I in 1 .. 15 loop
			buflen.Append(bv.Element(C));
			bv.Next(C);
		end loop;
		bitlen := To_Integer(buflen);
		count := 0;
		curpos := bv.To_Index(C);
		loop
			exit when count >= bitlen;
			total := total +  Decode_Packet(C);
			newpos := bv.To_Index(C);
			count := count + (newpos - curpos);
		end loop;
		return total;
	end Decode_Operator_0;

	function Decode_Operator_1(C: in out bv.Cursor) return Integer is
		buflen : bv.Vector := bv.Empty_Vector;
		subpacks : Integer := 0;
		total : Integer := 0;
	begin
		for I in 1 .. 11 loop
			buflen.Append(bv.Element(C));
			bv.Next(C);
		end loop;
		subpacks := To_Integer(buflen);
		for I in 1 .. subpacks loop
			total := total + Decode_Packet(C);
		end loop;
		return total;
	end Decode_Operator_1;

	function Decode_Packet(cur: in out bv.Cursor) return Integer is
		version_packet, type_packet : Integer := 0;
	begin
		version_packet := Read_Version_Type(cur);
		type_packet := Read_Version_Type(cur);
		if type_packet = 4 then
			return version_packet + Decode_Literal(cur);
		else
			if bv.Element(cur) then
				bv.Next(cur);
				return version_packet + Decode_Operator_1(cur);
			else
				bv.Next(cur);
				return version_packet + Decode_Operator_0(cur);
			end if;
		end if;
	end Decode_Packet;

	function Decode_Packet(s: string) return Integer is
		packbits : bv.Vector := Packet_Bits(s);
		cur : bv.Cursor := packbits.To_Cursor(packbits.First_Index);
	begin
		return Decode_Packet(cur);
	end Decode_Packet;
end Packet_Decoders;
