package application.messenger is
	type Message is tagged private;
	type Reference is access all Message'class;
	function Require_not_empty return Boolean;
	function Ensure_length_adjusted return Boolean;
	function Require_not_full return Boolean;
	procedure Make(M: in out Message; S: String);
	procedure Get(
		M: out Message;
		Precondition : Assertion := Require_not_empty'access;
		Postcondition : Assertion := Ensure_length_adjusted'access);
	procedure Put(
		M: in Message;
		Precondition : Assertion := Require_not_full'access;
		Postcondition : Assertion := Ensure_length_adjusted'access);

	private
	type Message is tagged record
		Text: String(1..120);
		Length : Natural := 0;
	end record;
end application.messenger;
