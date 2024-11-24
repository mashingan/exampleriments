with Varsize_Records; use Varsize_Records;
with Ada.Text_IO; use Ada.Text_IO;

procedure Varsize_Records_User is
    procedure Print_Stack (G :Growable_Stack_Discriminant) is
    begin
        Put("<Stack, items: [");
        for I in G.Items'range loop
            exit when I > G.len;
            Put(Integer'image(G.Items(I)) &
            (if I >= G.len then "" else ","));
        end loop;
        Put_Line("]>");
    end Print_Stack;

    S : Growable_Stack_Discriminant := (
        Max_Len => 128,
        Items => (1, 2, 3, 4, others => <>),
        Len => 4);
begin
    Print_Stack(S);
end;

