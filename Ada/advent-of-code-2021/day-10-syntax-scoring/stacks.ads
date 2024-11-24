generic
Max : Positive;
type Element_Type is private;
package Stacks is
	type Stack is limited private;

	Stack_Underflow, Stack_Overflow: Exception;

	function Is_Empty(S: in Stack) return Boolean;
	function Pop(S: in out Stack) return Element_Type;
	procedure Push(S: in out Stack; Val: Element_Type);
	procedure Empty(S: in out Stack);

	private

	type Stack_Array is array (Natural range <>) of Element_Type;

	Min : Constant := 0;

	type Stack is record
		Container : Stack_Array(Min .. Max);
		Top : Natural := Min;
	end record;
end Stacks;
