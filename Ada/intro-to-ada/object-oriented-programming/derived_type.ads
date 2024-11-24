package Derived_Type is
    type Point is record
        X , Y : integer;
    end record;

    type New_Point is new Point;
end Derived_Type;
