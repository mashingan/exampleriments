package Orders is
    type Fruit is (Banana, Orange, Pear);
    type Money is delta 0.01 digits 6;

    type Bill is tagged private;

    procedure Add (
        Slip: in out Bill;
        Kind: in Fruit;
        Amount: in Natural);

    function How_Much (Slip: Bill) return Money;

    procedure Pay (
        Ordered: in out Bill;
        Giving: in Money);

    private
    type Bill is tagged record
        Sum : Money := 0.0;
    end record;
end Orders;
