package Points is

    type Point is record
        X, Y : Integer := 0;
    end record;

    type Point_Array is array (Positive range <>) of Point;

    -- use the default values
    Origin      : Point := (X | Y => <>);
    Origin_2    : Point := (Others=> <>);

    Points_1 : Point_Array := ((1, 2), (3, 4));
    Points_2 : Point_Array := (
        1 => (1, 2),
        2 => (3, 4),
        3 .. 20 => <>);
end Points;
