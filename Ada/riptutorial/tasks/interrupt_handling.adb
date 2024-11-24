with Ada.Text_IO; use Ada.Text_IO;
with Ctl_C_Handling; use Ctl_C_Handling;

procedure Interrupt_handling is
begin
    delay 40.0;
    CTL_Reporter.Stop;
    Put_Line("Program ended");
end Interrupt_handling;
