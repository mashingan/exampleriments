-- compile with: gnatmake -I.. simple_decl
with Ada.Text_IO; use Ada.Text_IO;
with myutils; use myutils;

procedure simple_decl is
    type My_int is range 0 .. 1000;
    type Index is range 1 .. 5;
    type My_Int_array is array (index) of My_int;
    arr : My_Int_array := (2, 3, 5, 7, 11);

    type AnotherIndex is range 11 .. 15;
    type My11_15 is array (AnotherIndex) of My_int;
    tab : My11_15 := (2, 3, 5, 7, 11);

    type Month is (
        Jan, Feb, Mar, Apr, May, Jun, 
        Jul, Aug, Sep, Oct, Nov, Dec);
    type Month_Duration is range 1 .. 31;
    type Month_Array is array (Month) of Month_Duration;
    tabmonth : constant Month_Array := (
        --    v ignoring leap years
        31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
    Feb_Days : Month_Duration := tabmonth(Feb);

    type SimplerDecl is array (1 .. 5) of My_int;
    tabsimpler : SimplerDecl := (2, 3, 5, 7, 11);

begin
    Put("0 .. 5: index range:");
    for I in index loop
        Put(My_int'image (arr (I)));
    end loop;
    New_Line;

    Put("11 .. 15 index range:");
    for I in AnotherIndex loop
        Put(My_int'image (tab (I)));
    end loop;
    New_Line;

    for M in Month loop
        Put_Line(
            CapFirst(M'image) & " has" &
            Month_Duration'image(TabMonth(M)) & " days.");
    end loop;

    Put("anonymous 1..5 index range:");
    for I in tabsimpler'range loop
        Put(My_int'image (tabsimpler (I)));
    end loop;
    New_Line;

    Put("anonymous 1..5 fine-grained range:");
    for I in tabsimpler'first .. tabsimpler'last-1 loop
        Put(My_int'image (tabsimpler (I)));
    end loop;
    New_Line;

    Put_Line("Length of tabsimpler is" & Integer'image(tabsimpler'length));
    Put("tabsimpler index range num:");
    for I in tabsimpler'first .. tabsimpler'last loop
        Put(I'image);
    end loop;
    New_Line;
    Put("tab (11_15) index range num:");
    for I in tab'first .. tab'last loop
        Put(I'image);
    end loop;
end;
