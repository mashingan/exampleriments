procedure discriminated_records is
type Square(X: Positive) is
    record
        S: Matrix(1 .. X, 1 .. X);
    end record;

type Fruit is (Banana, Orange, Pear, Melon);

type Basket (Kind : Fruit) is
    record
        case Kind is
            when Banana =>
                Bunch_size : Positive;
                Bunch_per_box : Natural;
            when Pear | Orange =>
                Fruits_Per_Box : Natural;
            when Melon =>
                null;
        end case;
    end record;

Box : Basket(Banana);
begin
    null;
end discriminated_records;
