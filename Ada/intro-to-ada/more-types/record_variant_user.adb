with Record_Variant; use Record_Variant;
with Ada.Text_IO; use Ada.Text_IO;

procedure Record_Variant_User is
    function Eval_Expr(E : Expr) return integer is
        (case E.Kind is
        when Bin_op_plus => Eval_Expr(E.left.all) + Eval_Expr(E.right.all),
        when Bin_op_Minus => Eval_Expr(E.left.all) - Eval_Expr(E.right.all),
        when Num => E.val);

    E : Expr := (
        Bin_op_plus,
        new Expr'(Bin_op_Minus,
                  new Expr'(Num, 12), new Expr'(Num, 15)),
        new Expr'(Num, 3));

begin
    Put(Integer'image(Eval_Expr(E)));
end;
