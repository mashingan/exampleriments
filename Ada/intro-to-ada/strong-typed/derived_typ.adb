with Ada.Text_IO; use Ada.Text_IO;

procedure derived_typ is
    type Social_Security_Number is new Integer range 0 .. 999_99_9999;

    SSN : Social_Security_Number := 555_55_555;
    I : Integer;

    --Invalid_SSN : Social_Security_Number := -1;
    --                                      ^ Will cause runtime error
    --                                      and compile-time warning

    type Days is (
        Monday, Tuesday, Wednesday, Thursday,
        Friday, Saturday, Sunday);

    type Weekend_Days is new Days range Saturday .. Sunday;

begin
    --I := SSN;
    --SSN := I;
    --above two are illegals because different type
    I := Integer (SSN);
    SSN := Social_Security_Number(I);
end;
