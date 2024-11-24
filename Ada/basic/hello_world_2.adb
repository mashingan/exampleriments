with Ada.Text_IO;

procedure Hello is
    package IO renames Ada.Text_IO;
begin
    IO.put_line("hello world!");
    io.new_line;
    io.put_line("I'm an Ada program with package rename.");
end Hello;
