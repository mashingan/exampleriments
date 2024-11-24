with Interfaces.C; use Interfaces.C;

procedure Interfacing_C is
    type C_Enum is (A, B, C) with Convention => C;
    -- use C Convention for enum

    type c_struct is record
        a: int;
        b: long;
        c: unsigned;
        d: double;
    end record with Convention => C;
    -- the corresponding data types in C
    --struct c_struct
    --{
    --  int     a;
    --  long    b;
    --  unsigned c;
    --  double  d;
    --};

    function my_func(a: int) return int
        with
            Import      => true,
            Convention  => C;
    -- Import function 'my_func' from C.
    -- And now it's can be called from Ada.
    -- the corresponding declaration in C is int my_func(int a);

    function doubler(a: int) return int with
        Import      => true,
        Convention  => C,
        External_Name => "my_func";
    
begin
    null;
end Interfacing_C;
