package Stacks is
    type Stack is private;
    type StackLimited is limited private;

    procedure Push(S : in out Stack; val: Integer);
    procedure Pop(S : in out Stack; val: out Integer);
    procedure Push(S : in out StackLimited; val: Integer);
    procedure Pop(S : in out StackLimited; val: out Integer);

    private
    subtype stack_index is natural range 1 .. 10;
    type content_type is array (Stack_Index) of Natural;

    type Stack is record
        Top : Stack_Index := Stack_Index'first;
        Content : Content_Type;
    end record;
    type StackLimited is limited record
        Top : Stack_Index := Stack_Index'first;
        Content : Content_Type;
    end record;
end Stacks;
