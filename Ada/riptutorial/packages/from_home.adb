package Orders.From_Home is
    type Address is new String (1 .. 120);

    type Ordered_By_Phone is new Bill with private;

    procedure Deliver (
        Ordered : in out Ordered_By_Phone;
        Place : in Address);

    private
    type Ordered_By_Phone is new Bill with
        record
            Delivered : Boolean := false;
            To : Address;
        end record;
end Orders.From_Home;
