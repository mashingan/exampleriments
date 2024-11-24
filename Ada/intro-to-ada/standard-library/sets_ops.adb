with Ada.Containers;
with Ada.Containers.Ordered_Sets;

with Ada.Text_IO; use Ada.Text_IO;

procedure sets_ops is
    package cters renames Ada.Containers;
    package Integer_Sets is new Ada.Containers.Ordered_Sets
    (Element_Type => Integer);

    use Integer_Sets;

    S : Set;
    C : Cursor;
    Ins : Boolean;

    procedure Report_Set(S : Set) is
    begin
        Put_Line("Set has " & cters.Count_Type'image(S.Length) & " elements.");
    end;

    procedure LoopElement(S : Set) is
    begin
        Put("{");
        for E of S loop
            Put(E'image);
        end loop;
        Put_Line("}");
    end;
begin
    S.Insert(20);
    S.Insert(10);
    S.Insert(0);
    S.Insert(13);

    S.Insert(0, C, Ins);
    if not Ins then
        Put_Line("Inserting 0 into the set was not successful");
    end if;

    -- use include to ignore if the value already available in set
    S.Include(0);
    S.Include(13);
    S.Include(14);

    Report_Set(S);
    LoopElement(S);

    S.Delete(13);
    --S.Delete(13); -- the 2nd deletion will error
    S.Exclude(13);  -- ignore the status operation
    LoopElement(S);

    if S.Contains(20) then
        Put_Line("Found element 20 in set.");
    end if;
    C := S.Find(0);
    if C = No_Element then
        Put_Line("Could not find element 0 within set.");
    else
        Put_Line("Found the element 0 within set.");
    end if;

end sets_ops;
