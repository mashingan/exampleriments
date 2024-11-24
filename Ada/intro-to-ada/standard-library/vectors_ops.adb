with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;

procedure vectors_ops is
    package cntner renames Ada.Containers;
    package Integer_Vectors is new Ada.Containers.Vectors
    (Index_Type => Natural, Element_Type => Integer);

    use Integer_Vectors;

    V : Integer_Vectors.Vector;
    V2 : Vector := 20 & 10 & 0 & 13;

    procedure report_vecops (V : Vector) is
    begin
        Put_Line("Vector has " & cntner.Count_Type'image(V.length) &
        " elements");
    end;

    function Img(I : Integer) return String renames Integer'Image;
    function Img(I : cntner.Count_Type) return String
        renames cntner.Count_Type'Image;

    procedure FirstLastRep(V : Vector) is
    begin
        Put_Line("First element is " & Img(V.First_element));
        Put_Line("Last element is " & Img(V.Last_element));
    end FirstLastRep;

    procedure LoopElement (V: Vector) is
    begin
        Put("<#");
        for E of V loop
            Put(Img(E));
        end loop;
        Put_Line("#>");
    end;

    procedure IterElement (V: Vector) is
    begin
        Put("<#");
        for I in V.First_Index .. V.Last_Index loop
            Put(Integer'image(V(I)) & (
                if I = V.Last_Index then "" else " "));
        end loop;
        Put_Line("#>");
    end;

    procedure ExtendedPrint (V : Vector) is
    begin
        for I in V.First_Index .. V.Last_Index loop
            Put("V[" & Extended_Index'image(I) & "] =");
            Put_Line(Integer'image(V(I)));
        end loop;
    end;

    procedure CursorIter (V : Vector) is
    begin
        for C in V.Iterate loop
            Put("V[" & Extended_Index'image(To_Index(C)) & "] =");
            Put_Line(Integer'image(V(C)));
        end loop;
    end;
    
    generic
    type T is private;
    with function BinOp (X, Y: T) return T;
    procedure Generic_BinOp(I : in out T; X : T);
    procedure Generic_BinOp(I : in out T; X : T) is
    begin
        I := BinOp(I, X);
    end;

    procedure Add_Integer is new Generic_BinOp(T => Integer,
    BinOp => Standard."+");
    procedure Sub_Integer is new Generic_BinOp(T => Integer,
    BinOp => Standard."-");

    procedure Add_one (I : in out Integer) is
    begin
        Add_Integer(I, 1);
    end;
    procedure Sub_One (I : in out Integer) is
    begin
        Sub_Integer(I, 1);
    end;

    procedure Add_One_wCursor (V : in out Vector) is
    begin
        for C in V.Iterate loop
            V.Update_Element(C, Add_One'Access);
        end loop;
    end Add_One_wCursor;
    procedure Sub_One_wCursor (V : in out Vector) is
    begin
        for C in V.iterate loop
            V.Update_Element(C, Sub_One'access);
        end loop;
    end Sub_One_wCursor;

    procedure Find_And_Change_Elem(V: in out Vector) is
        Idx : Extended_Index;
        C : Cursor;
    begin
        Idx := V.Find_Index(11);
        Put_Line("Index of an Element with value 11 is " &
        Extended_Index'image(idx));

        if Idx /= No_Index then
            V(Idx) := 10;
        end if;

        C := V.Find(14);
        Idx := To_index(C);
        Put_Line("Index of an Element with value 14 is " &
        Extended_Index'image(idx));
        if C /= No_Element then
            V(C) := 13;
        end if;

        Idx := V.Find_Index(100);
        Put_Line("No_Index:" & Idx'image);
    end Find_And_Change_Elem;

    function Is_Even(N : Integer) return Boolean is (N mod 2 = 0);
    procedure Remove_With(
        V : in out Vector;
        Test : not null access function (N : Integer) return boolean) is

        Idx : Integer := 0;
    begin
        loop
            exit when (V.Is_Empty or Idx > V.Last_Index);
            if Test(V(Idx)) then
                V.Delete(Idx);
            else
                Idx := Idx + 1;
            end if;
        end loop;
    end Remove_With;

    procedure Remove_Specific(V: in out Vector; N : Integer) is
        C : Cursor;
    begin
        loop
            C := V.Find(N);
            exit when C = No_Element;
            V.Delete(C);
        end loop;
    end;

    Package Integer_Vectors_Sorting is new Integer_Vectors.Generic_Sorting;
    use Integer_Vectors_Sorting;

begin
    report_vecops (V);
    report_vecops (V2);
    New_Line;

    Put("Appending some element to the vector...");
    v.append(20);
    v.append(10);
    v.append(0);
    v.append(13);
    Put_Line("done!");
    Put("Prepending some element to the vector...");
    v.prepend(30);
    v.prepend(40);
    v.prepend(100);
    Put_Line("done!");
    report_vecops(V);

    New_Line;
    FirstLastRep(V);

    New_Line;
    V.Swap(V.First, V.Last);
    Put_Line("First-Last element swapped!: ");
    FirstLastRep(V);

    New_Line;
    Put("LoopElement vector: ");
    LoopElement(V);
    Put("IterElement vector: ");
    IterElement(V);
    ExtendedPrint(V);
    New_Line;
    CursorIter(V);

    New_Line;
    Put("Add_One_wCursor: now: ");
    Add_One_wCursor(V);
    LoopElement(V);

    New_Line;
    Find_And_Change_Elem(V);
    Put("Now: "); LoopElement(V);

    New_Line;
    Put("Insert 9 just before the end: now: ");
    V.insert(V.Last_Index, 9, Count => 1);
    LoopElement(V);

    Put("Now find and delete element with value 21: ");
    declare
        C : Cursor;
    begin
        C := V.Find(21);
        if C /= No_Element then
            Put("found at " & Extended_Index'image(To_Index(C)) & ", now: ");
            V.Delete(C);
        end if;
    end;
    LoopElement(V);
    Put("Dec by 1 for all: now ");
    Sub_One_wCursor(V);
    LoopElement(V);
    Put("Delete all even elements: now: ");
    Remove_With(V, Is_Even'access);
    LoopElement(V);
    Put("V2: "); LoopElement(V2);
    Put("Concatenate V and V2 results in: ");
    V := V & V2;
    LoopElement(V);
    Put("Sorting V: ");
    Sort(V);
    LoopElement(V);
    Put("Merge with V2: ");
    Merge(V, V2);
    LoopElement(V);

end vectors_ops;
