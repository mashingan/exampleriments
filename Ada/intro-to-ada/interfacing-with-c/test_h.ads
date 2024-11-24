pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package test_h is

   type test is null record;   -- incomplete struct

   function test_create return access test  -- test.h:3
   with Import => True, 
        Convention => C, 
        External_Name => "test_create";

   procedure test_destroy (arg1 : access test)  -- test.h:5
   with Import => True, 
        Convention => C, 
        External_Name => "test_destroy";

   procedure test_reset (arg1 : access test)  -- test.h:7
   with Import => True, 
        Convention => C, 
        External_Name => "test_reset";

   procedure test_set_name (arg1 : access test; arg2 : Interfaces.C.Strings.chars_ptr)  -- test.h:9
   with Import => True, 
        Convention => C, 
        External_Name => "test_set_name";

   procedure test_set_address (arg1 : access test; arg2 : Interfaces.C.Strings.chars_ptr)  -- test.h:11
   with Import => True, 
        Convention => C, 
        External_Name => "test_set_address";

   procedure test_display (arg1 : access constant test)  -- test.h:13
   with Import => True, 
        Convention => C, 
        External_Name => "test_display";

end test_h;
