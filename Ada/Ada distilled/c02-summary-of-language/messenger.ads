package Messenger is
	type Message is private;
	function Create(S: String) return Message;
	procedure Send (M: in Message);
	procedure Receive(M: out Message);
	function Size(M: in Message) return Natural;

	private
	type Message is record
		Text: String(1 .. 120) := (others => '');
		Length: Natural := 0;
	end record;
end Messenger;

with Ada.Calendar;
package Messenger.Dated is
	type Dated_Message is private;
	function Create(M: in Message)
		return Dated_Message;

	private
	type Dated_Message is record
		Text: Message;
		Date: Ada.Calendar.Time;
	end record;
end Messenger.Dated;
