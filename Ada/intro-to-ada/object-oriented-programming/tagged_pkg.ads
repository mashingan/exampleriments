package tagged_pkg is
    type My_Class is tagged null record;
    -- jsut like a regular record but with tagger qualifier

    procedure Foo (self : in out My_Class);

    type Derived is new My_Class with record
        A : Integer;
    end record;

    overriding procedure Foo (self : in out Derived);
end tagged_pkg;
