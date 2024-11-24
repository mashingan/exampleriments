with Ada.Text_IO; use Ada.Text_IO;

package body tagged_pkg is
    procedure Foo (self : in out My_Class) is
    begin
        Put_Line("In my class.Foo");
    end Foo;

    procedure Foo(self : in out Derived) is
    begin
        Put_Line("In Derived_Type.Foo, A = " & self.A'image);
    end Foo;
end tagged_pkg;
