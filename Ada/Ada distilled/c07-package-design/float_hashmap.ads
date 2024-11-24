with Ada.Containers.Doubly_Linked_Lists;

package Float_hashmap is
	type Map is private;
	procedure Insert(M: in out Map, Key: Float, Val: Integer);
	function Contains(M: in Map, Key: Float) return Boolean;
	function "()"(M: in Map, Key: Float) return Integer;

	private
	type Key_Index is mod 64;
	package DLinked_list is new Ada.Containers.Doubly_Linked_Lists
	(Element_Type => Float);
	type Data is array(Key_Index) of DLinked_list.List;
	type Map is record
		Map_Data: Data;
	end record;
end Float_hashmap;
