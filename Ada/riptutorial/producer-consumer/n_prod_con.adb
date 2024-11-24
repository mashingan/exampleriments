with Ada.Text_IO; use Ada.Text_IO;

procedure N_Prod_Con is
    protected Buffer is
        Entry Write(Item: in Integer);
        Entry Read(Item: out Integer);
    private
        Value : Integer := Integer'Last;
        Is_new : Boolean := false;
    end Buffer;

    protected body Buffer is
        Entry Write(Item : in Integer) when not Is_new is
        begin
            Value := Item;
            Is_new := True;
        end Write;
        Entry Read(Item : out Integer) when Is_new is
        begin
            Item := Value;
            Is_new := false;
        end Read;
    end Buffer;

    task type Producers(Id: Positive) is
        Entry Stop;
    end Producers;
    task body Producers is
        Num : Positive := 1;
    begin
        loop
            select
                accept stop;
                exit;
            or
                delay 0.0001;
            end select;
            Put_Line("Producers" & Integer'Image(Id) &
                " writing " & Integer'Image(Num));
            Buffer.Write(Num);
            Num := Num + 1;
        end loop;
    end Producers;

    task type Consumers(Id: Positive) is
        Entry Stop;
    end Consumers;

    task body Consumers is
        Num : Integer;
    begin
        loop
            select
                accept stop;
                exit;
            or
                delay 0.0001;
            end select;
            Buffer.Read(Num);
            Put_Line("Consumer" & Integer'Image(Id) & " read " &
                Integer'Image(Num));
        end loop;
    end Consumers;
    P1 : Producers(1);
    P2 : Producers(2);
    P3 : Producers(3);
    C1 : Consumers(1);
    C2 : Consumers(2);
    C3 : Consumers(3);
begin
    delay 0.2;
    P1.Stop;
    P2.Stop;
    P3.Stop;
    C1.Stop;
    C2.Stop;
    C3.Stop;
end N_Prod_Con;
