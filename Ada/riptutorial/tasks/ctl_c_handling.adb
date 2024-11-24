with Ada.Text_IO; use Ada.Text_IO;
with Ctl_C_Handling; use Ctl_C_Handling;
with Ada.Calendar; use Ada.Calendar;

package body Ctl_C_Handling is
    protected body Ctl_C_Handler is
        procedure Handle_Int is
        begin
            Pending_Int_Count := Pending_Int_Count + 1;
        end Handle_int;

        entry Wait_For_Int when Pending_Int_Count > 0 is
        begin
            Pending_Int_Count := Pending_Int_Count - 1;
        end Wait_For_Int;
    end Ctl_C_Handler;

    task body CTL_Reporter is
        type Second_Bin is mod 10;
        type History is array(Second_Bin) of Natural;

        procedure Display_History(Item : History) is
            sum : Natural := 0;
        begin
            for I in Item'Range loop
                Put_Line("Second: " & Second_Bin'Image(I) & " :" &
                Natural'Image(Item(I)));
                Sum := Sum + Item(I);
            end loop;
            Put_Line("Total count: " & Natural'Image(Sum));
            New_Line(2);
        end Display_History;

        One_Second_Count : Natural := 0;
        Next_Slot : Second_Bin := 0;
        Next_Second : Time := Clock + 1.0;
        Ten_second_history : History := (Others => 0);

    begin
        loop
            select
                accept stop;
                exit;
            else
                select
                    Ctl_C_Handler.Wait_For_Int;
                    One_Second_Count := One_Second_Count + 1;
                or
                    delay until Next_Second;
                    Next_Second := Next_Second + 1.0;
                    Ten_second_history(Next_Slot) := One_Second_Count;
                    Display_History(Ten_second_history);
                    Next_Slot := Next_Slot + 1;
                    One_Second_Count := 0;
                end select;
            end select;
        end loop;
    end CTL_Reporter;
end Ctl_C_Handling;
