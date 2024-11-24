with Ada.Text_IO; use Ada.Text_IO;
with tagged_pkg; use tagged_pkg;

procedure classwide_use_tagged is
    o1 : My_Class;
    o2 : Derived := (A => 12);
    o3 : My_Class'class := o2;
    o4 : My_class'class := o1;
    o32 : My_class := My_Class (o2);

    package extended is
        type D2 is new Derived with null record;

        procedure Bar (Self : in out D2; val : Integer);
    end extended;

    package body extended is
        procedure Bar (Self : in out D2; Val : integer) is
        begin
            Self.A := Self.A + val;
        end Bar;
    end extended;

    use extended;

    obj2 : D2 := (A => 15);
begin
    Foo (o1);   -- non dispatching to My_Class.Foo
    Foo (o2);   -- non dispatching to Derived_Type.Foo
    Foo (o3);   -- Dispatching calls Derived_Type.Foo
    Foo (o4);   -- Dispatching calls My_Class.Foo
    Foo (o32);  -- same as o3

    -- using dot notation
    New_Line;
    o1.Foo;
    o2.Foo;
    o3.Foo;
    o4.Foo;

    Obj2.Bar (2);
    Obj2.Foo;
end;
