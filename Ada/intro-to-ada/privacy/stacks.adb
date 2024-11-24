package body Stacks is
    procedure Push(S : in out Stack; val: Integer) is
    begin
        S.Top := S.Top + 1;
        S.Content(S.Top) := val;
    end;

    procedure Pop(S : in out Stack; val: out Integer) is
    begin
        val := S.Content(S.Top);
        S.Top := S.top - 1;
    end;

    procedure Push(S : in out StackLimited; val: Integer) is
    begin
        S.Top := S.Top + 1;
        S.Content(S.Top) := val;
    end;
    procedure Pop(S : in out StackLimited; val: out Integer) is
    begin
        val := S.Content(S.Top);
        S.Top := S.top - 1;
    end;
end Stacks;
