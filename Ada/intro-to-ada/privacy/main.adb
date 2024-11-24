with stacks; use stacks;
with Ada.Text_IO; use Ada.Text_IO;

procedure main is
    S : Stack;
    Res: Integer;
    S1, S2: StackLimited;
begin
    Push (S, 5);
    Push (S, 7);
    Pop (S, Res);
    Put("The res value: " & Res'image);
end main;
