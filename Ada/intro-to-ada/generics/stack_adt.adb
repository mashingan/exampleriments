with Ada.Text_IO; use Ada.Text_IO;

procedure stack_adt is
    generic
    Max : Positive;
    type T is private;
    package Stacks is
        type Stack is limited private;

        Stack_Underflow, Stack_Overflow : Exception;

        function Is_Empty (S : Stack) return Boolean;
        function Pop (S : in out Stack) return T;
        procedure Push (S : in out Stack; Val : T);

        private

        type Stack_array is array (Natural range <>) of T;

        Min : Constant := 1;

        type Stack is record
            Container : Stack_array(Min .. Max);
            Top : Natural := Min - 1;
        end record;
    end Stacks;

    package body stacks is
        function Is_Empty (S : Stack) return Boolean is
            (S.Top < S.Container'First);

        function Is_Full (S : Stack) return Boolean is
            (S.Top >= S.Container'Last);

        function Pop (S : In Out Stack) return T is
        begin
            if Is_Empty(S) then
                raise Stack_Underflow;
            else
                return X : T do
                    X := S.Container (S.Top);
                    S.Top := S.Top - 1;
                end return;
            end if;
        end Pop;

        procedure Push (S : In out Stack; Val: T) is
        begin
            if Is_Full(S) then
                raise Stack_Overflow;
            end if;
            S.Top := S.Top + 1;
            S.Container (S.Top) := Val;
        end Push;
    end Stacks;

    package IStack is new Stacks (Max => 10, T => Integer);

    Values : IStack.Stack;

begin
    IStack.Push(Values, 10);
    IStack.Push(Values, 20);
    Put_Line("Last value inputted was " & Integer'image(IStack.Pop (Values)));
end stack_adt;
