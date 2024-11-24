private package application.messenger.storage is
	type Message_Container is private;
	procedure Insert_at_end(Into: in out Message_Container; M: in Message'class);
	procedure Get_first_message(From: in out Message_Container; M: out Message'class);
	private
	type Container is array(1..500) of Reference;
	type Message_Container is record
		Data: Container;
		Count : Natural := 0;
	end record;
end application.messenger.storage;
