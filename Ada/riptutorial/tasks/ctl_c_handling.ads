with Ada.Interrupts.Names; use Ada.Interrupts.Names;

package Ctl_C_Handling is
    protected Ctl_C_Handler is
        procedure Handle_Int with
            Interrupt_Handler,
            Attach_Handler => SIGINT;
        entry Wait_For_Int;
        private
        Pending_Int_Count : Natural := 0;
    end Ctl_C_Handler;

    task CTL_Reporter is
        entry stop;
    end CTL_Reporter;
end Ctl_C_Handling;
