with application.messenger.storage;

package body application.messenger is
	package amstorage renames application.messenger.storage;
	The_storage_container : amstorage.Message_Container;

	procedure Get(
		M: out Message;
		Precondition : Assertion := Require_not_empty'access;
		Postcondition : Assertion := Ensure_length_adjusted'access) is
	begin
		if Require_not_empty then
			storage.Get_first_message
			(From => The_storage_container, M => M);
		end if;
		if Ensure_length_adjusted then
			null;
		end if;
	end Get;

	function Len(M: in Message) return Natural is
	begin
		return M.length;
	end Len;

	procedure Make(M: in out Message; S: String) is
	begin
		if S'length > M.Text'length then
			raise Precondition_error;
		else
			M.Text(1..S'length) := S(S'range);
		end if;
		M.length := S'length;
	end Make;

	procedure Put(
		M: in Message;
		Precondition : Assertion := Require_not_full'access;
		Postcondition : Assertion := Ensure_length_adjusted'access) is
	begin
		amstorage.Insert_at_end(
			Into => The_storage_container,
			M => M);
	end Put;

	function Require_not_empty return Boolean is
	begin
		return False;
	end Require_not_empty;

	function Ensure_length_adjusted return Boolean is
	begin
		return False;
	end Ensure_length_adjusted;

	function Require_not_full return Boolean is
	begin
		return False;
	end Require_not_full;

end application.messenger;
