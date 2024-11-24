package body record_use is
	M4 : Message_4;
	M5 : Message_5;
	procedure Display(The_Data: Message_4) is
	begin
		null;
	end Display;
	procedure Display(The_data: Message_5) is
	begin
		null;
	end Display;
	--procedure Print(Data: Message_4'class) is
	--begin
		--Display(Data);
	--end Print;
	procedure Process(Data: in out Message_4) is
	begin
		--Print(data);
		Display(data);
	end Process;
begin
	Process(Message_4(M5)); -- no changing data nor tag
end record_use;
