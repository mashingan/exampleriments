--compile:
--$ gcc -c test.c
--$ gnatmake use_binding_test.ad -largs test.o

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with test_h; use test_h;

with System;

procedure use_binding_test is
    Name : Constant chars_ptr := New_String("John Doe");
    Address : Constant chars_ptr := New_String("Small Town");

    T : access test := test_create;

begin
    test_reset(T);
    test_set_name(T, name);
    test_set_address(T, address);
    
    test_display(T);
    test_destroy(T);
end use_binding_test;
