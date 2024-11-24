with Ada.Text_IO; use Ada.Text_IO;

procedure declare_greet is
begin
    loop
        Put("Please enter your name: ");

        declare
            Name : string := Get_Line;
        begin
            exit when Name = "";
            Put_Line("Hi " & name & "!");
        end;
    end loop;
    Put_Line("bye!");
end declare_greet;
