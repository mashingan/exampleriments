with Ada.Text_IO; use Ada.Text_IO;

procedure strong_typ is
    type Meters is new float;
    type Miles is new float;
    Dist_Imperial : Miles;
    Dist_Metric : constant meters := 100.0;

    function To_Miles(M : Meters) return Miles is
    begin
        return (Miles (M) * 1609.0) / 1000.0;
    end To_Miles;
begin
    Dist_Imperial := (Miles (Dist_Metric) * 1609.0) / 1000.0;
    --                ^ type conversion, meter → miles

    Put_Line(Miles'image(Dist_Imperial));

    Put_Line(Miles'image(To_Miles (Meters (1000.0))));
    Put_Line(Miles'image(To_Miles (1000.0)));
end;
