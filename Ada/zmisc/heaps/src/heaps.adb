package body Heaps is
    function Left (idx : in Natural) return Natural is (2 * idx + 1);
    function Right (idx : in Natural) return Natural is (2 * idx + 2);
    function Parent (idx : in Natural) return Integer is ((idx - 1) / 2);

    procedure Add (self : in out Heap; Val : Element_Type) is
        prev_length : Natural := 0;
    begin
        prev_length := Natural (self.value.Length);
        self.value.Append (Val);
        self.Bubble_Up (prev_length);
    end Add;

    procedure Bubble_Up (self : in out Heap; idx : in Natural) is
        pdx : Integer;
        val : nv.Vector renames self.value;
        newidx : Integer := idx;
    begin
        pdx := Parent (newidx);

        loop
            exit when newidx > 0 and not Priority (val (newidx), val (pdx));
            self.value.Swap (newidx, pdx);
            newidx := pdx;
            pdx := Parent (newidx);

        end loop;

    end Bubble_Up;

    procedure Remove_Priority (self : in out Heap) is
    begin
        self.value.Delete_First;
        self.Trickle_Down (0);
    end Remove_Priority;

    procedure Trickle_Down (self : in out Heap; idx : in Natural) is
         J : Integer;
         R : Natural;
         L : Natural;
         val : nv.Vector renames self.value;
         idx2 : Integer := idx;
    begin
        loop
            J := -1;
            R := Right (idx2);
            if r < Integer (val.Length) and Priority (val (R), val (idx2)) then
                L := Left (idx2);
                if Priority (val (L), val (R)) then
                    J := L;
                else
                    J := R;
                end if;
            else
                L := Left (idx2);
                if L < Integer (val.Length) and Priority (val (L), val (idx2)) then
                    J := L;
                end if;
            end if;
            if J >= 0 then val.Swap (idx2, J); end if;
            idx2 := J;
            exit when idx2 < 0;
        end loop;
    end Trickle_Down;

end Heaps;