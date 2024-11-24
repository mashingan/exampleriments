package MyUtils is
    function CapFirst (s: string) return string;

    type Days is (
        Monday, Tuesday, Wednesday, Thursday,
        Friday, Saturday, Sunday);

    subtype Day_Name is String (1 .. 2);
    -- subtype of string with know size

    type Days_Name_Type is array (Days) of Day_Name;

    type Month is (
        January, February, March, April, May, June,
        July, August, September, October, November, December);

    type Month_Short is (
        Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);

    type Date is record
        Day : Integer range 1 .. 31;
        Month : Month_Short;
        Year : Integer;
    end Record;

    generic
    type T is private;
    procedure Generic_Swap (X, Y : in out T);

end MyUtils;
