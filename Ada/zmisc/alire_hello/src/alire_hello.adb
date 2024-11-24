with Ada.Text_IO;
with LibHello;

procedure Alire_Hello is
   package tio renames Ada.Text_IO;
begin
   --  tio.Put_Line ("Hello 異世界 using Alire crate pkg-mgmt");
   LibHello.Hello_World;
end Alire_Hello;