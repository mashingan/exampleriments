with Ada.Strings.Unbounded;
with Ada.Calendar;

package Record_Decl is
	package sunbound renames Ada.Strings.Unbounded;
	type Library_Book is record
		ISBN: String(1..12);
		Title: String(1..30);
		Author: String(1..40);
		Purchase_Price: Float;
		Copies_Available : Natural;
	end record;

	type Message is record
		Text: sunbound.Unbounded_String;
		Length: Natural;
	end record;

	type Message_2(Size: Positive) is record
		Text: String(1..Size);
		Length: Natural;
	end record;

	type Message_3(Size: Positive := 1) is record
		Text: String(1..Size);
		Length: Natural;
	end record;

	type Message_4 is tagged record
		Text: sunbound.Unbounded_String;
		Length: Natural;
	end record;

	type Message_5 is new Message_4 with record
		Stamp: Ada.Calendar.Time;
	end record;

	type Message_6 is record
		Message_Data: Message;
		Library_Data: Library_Book;
	end record;
end Record_Decl;
