with Ada.Unchecked_Deallocation;

package body application.messenger.storage is
	procedure Free is new Ada.Unchecked_Deallocation
	(
		Object => Message'class,
		Name => Reference);
	procedure Get_first_message(From: in out Message_Container;
		M: out Message'class) is
		Work_container : Container := From.Data;
	begin
		M := From.Data(1).all;
		Free(From.Data(1));
		From.Data(1..From.Count) := From.Data(2..From.Data'last);
	end Get_first_message;

	procedure Insert_at_end(Into: in out Message_Container;
		M: in Message'class) is
	begin
		if Into.Count < Container'length then
			Into.Count := Into.Count + 1;
			Into.Data(Into.Count) := new Message'(M);
		else
			raise Precondition_error;
		end if;
	end Insert_at_end;
end application.messenger.storage;
