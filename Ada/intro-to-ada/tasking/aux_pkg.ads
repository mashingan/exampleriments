with Ada.Real_Time; use Ada.Real_Time;

package aux_pkg is
    function Get_Start_Time return Time with inline;
    procedure Show_Elapsed_Time with Inline;

    procedure Compute_Intensive;
    
    private
    Start_Time : Time := Clock;

    function Get_Start_Time return Time is (Start_Time);
end;
