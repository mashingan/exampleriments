package Qual_Expr is
    type Point is Record
        A, B : Integer;
    end record;

    P : Point := Point'(12, 15);
    A : Integer := Integer'(12);

    -- overloading with return type resolution
    type SSID is new integer;
    function Convert (Self : SSID) return Integer;
    function Convert (Self : SSID) return string;
    function Convert (Self : Integer) return string;
end Qual_Expr;
