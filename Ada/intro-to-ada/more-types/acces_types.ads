with myutils; use myutils;

package Access_Types is
    type Date_Acc is access Date;
    type Date_Acc_2 is access Date;

    D : Date_Acc := null;

    -- invalid! different type
    --D2 : Date_Acc_2 := D;

    D2 : Date_Acc_2 := null;

    D3 : Date_Acc_2 := Date_Acc_2(D);

    D_alloc : Date_Acc := new Date;

    type String_Acc is access String;
    Msg : String_Acc; -- default is null
    Buffer : String_Acc := new String(1 .. 10);
    --                               ^ Constraint required

    D30_Nov11 : Date_Acc := new Date'(30, November, 2011);
    HelloMsg : String_Acc := new String'("Hello");

    Today : Date := D30_Nov11.all;
    --                       ^ access value dereference

    J : Integer := D30_Nov11.Day;
    --                      ^ Implicit dereference for record and
    --                        array component, equivalent to
    --                        D30_Nov11.all.day
end Access_Types;
