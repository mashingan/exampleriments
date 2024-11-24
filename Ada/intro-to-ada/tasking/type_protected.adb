with Ada.Text_IO; use Ada.Text_IO;

procedure type_protected is
    protected type obj_type is
        procedure Set (V : Integer);
        function Get return Integer;
        entry Get (V: out Integer);
    private
        Local : integer := 0;
        Is_set : Boolean := false;
    end obj_type;

    protected body obj_type is
        procedure Set (V: Integer) is
        begin
            Local := V;
            Is_set := true;
        end set;

        function Get return Integer is (Local);

        entry Get (V: out integer)
        when Is_set is
            -- entry is blocked until the condition is true.
            -- The barrier is evaluated at call of entries and at exits
            -- of procedures and entries.
            -- The calling task sleeps until the barrier is released.
        begin
            V := local;
            is_Set := false;
        end get;
    end obj_type;

    obj : obj_type;
begin
    Obj.Set(5);
    declare
        N : integer;
    begin
        Obj.Get(N);
        Put_Line("Number is " & N'image);
    end;
end type_protected;
