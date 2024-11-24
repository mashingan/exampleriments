package Record_Variant is
    type Expr;
    type Expr_Access is access Expr;

    type Expr_Kind_Type is (Bin_op_plus, Bin_op_Minus, Num);

    type Expr (Kind : Expr_Kind_Type) is record
        case kind is
            when Bin_op_plus | Bin_op_Minus =>
                Left, Right : Expr_Access;
            when Num =>
                Val : Integer;
        end case;
    end record;
end Record_Variant;
