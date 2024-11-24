package body stacks is
	function Is_Empty(S: Stack) return Boolean is
		(S.Top < S.Container'First);
	function Is_Full(S: Stack) return Boolean is
		(S.Top >= S.Container'last);

	function Pop(S: in out Stack) return Element_Type is
	begin
		if Is_Empty(S) then
			raise Stack_Underflow;
		else
			return X : Element_Type do
				X := S.Container(S.Top);
				S.Top := S.Top - 1;
			end return;
		end if;
	end Pop;

	procedure Push(S: in out Stack; Val: Element_Type) is
	begin
		if Is_Full(S) then
			raise Stack_Overflow;
		end if;
		S.Top := S.Top + 1;
		S.Container(S.Top) := val;
	end Push;

	procedure Empty(S: in out Stack) is
	begin
		S.Top := Min;
	end Empty;
end stacks;
