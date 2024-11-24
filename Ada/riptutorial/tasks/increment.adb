with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure increment is
    use Ada.Text_IO;
    task Incrementor is
        entry Increment;
    end;
    task body Incrementor is
        use Ada.Integer_Text_IO;
        I : Integer := 0;
    begin
        loop
            accept Increment;
            I := I + 1;
            Put(I, 0);
            delay 0.1;
        end loop;
    end;
    K : Character;
begin
    loop
        Get_Immediate(K);
        if K in '0' .. '9' then
            for I in 1 .. Natural'Value(K & "") loop
                Incrementor.Increment;
            end loop;
        end if;
    end loop;
end;
