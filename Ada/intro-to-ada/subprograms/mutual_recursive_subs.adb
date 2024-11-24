with Ada.Text_IO; use Ada.Text_IO;

procedure mutual_recursive_subs is
    procedure compute_a (V : Natural);

    procedure compute_b (V : Natural) is
    begin
        Put_Line("in compute_b" & V'image);
        if V > 5 then
            Compute_A(V -1 );
        end if;
    end Compute_B;

    procedure compute_a (V : Natural) is
    begin
        Put_Line("in compute_a" & V'image);
        if V > 2 then
            Compute_B(V -1 );
        end if;
    end Compute_a;
begin
    Compute_A(15);
end mutual_recursive_subs;
