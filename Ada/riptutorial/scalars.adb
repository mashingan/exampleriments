procedure Scalars is
    type Fruit is (Banana, Orange, Pear, Melon);

    Choice : Fruit := Banana;

    type Roman_Numeral is (
        'I', 'V', 'X', 'L', 'C', 'D', 'M', Unknown);

    -- fixed point decimal
    type Money is delta 0.001 digits 10;
    Oil_Price : Money := 56.402;
    Loss : Money := 0.002 / 3; -- is 0.000

    -- fixed point ordinary
    Shoe_Ounce : constant := 2.54 / 64.0;
    type Thickness is delta Shoe_Ounce range 0.00 .. 1.00;

    Strop : Thickness := 0.1;

    -- floating point
    type Distance is digits 8;
    Earth : Distance := 40_075.017;

    -- modular integer
    type Bits is mod 2**24;
    L : Bits := 2#00001000_01010000_11001100# or 7;

    type Grade is range 0 .. 15;
    B : Grade := 11;
    C : Grade := 8;
    Avg : Grade := (B+C) / 2;
begin
    null;
end;
