with Ada.Text_IO; use Ada.Text_IO;

procedure protected_obj is
    protected obj is
        procedure Set (V : Integer);
        function Get return Integer;
        entry Get (V: out Integer);
    private
        Local : integer := 0;
        Is_set : Boolean := false;
    end obj;

    protected body obj is
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
    end obj;

    N : Integer := 0;

    task T;
    task body T is
    begin
        Put_Line("Task will delay for 4 seconds...");
        delay 4.0;
        Put_Line("Task T will set obj...");
        Obj.Set(5);
        Put_Line("Task T has just sent obj...");
    end T;
begin
    Put_Line("Main body will get obj...");
    Obj.Get(N);
    Put_Line("Main body just retrieved obj...");
    Put_Line("Number is: " & integer'image(N));
end;
