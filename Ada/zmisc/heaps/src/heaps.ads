with Ada.Containers.Vectors;

generic
    type Element_Type is private;
    with function Priority (e1, e2 : in Element_Type) return Boolean is <>;
    with function "=" (e1, e2 : in Element_Type) return Boolean is <>;
package Heaps is
    pragma Preelaborate;

    type Heap is tagged limited private;

    package Node_Vectors is new Ada.Containers.Vectors
       (Element_Type => Element_Type, Index_Type => Natural, "=" => "=");

    procedure Add (self : in out Heap; Val : Element_Type);
    procedure Remove_Priority (self : in out Heap);

private

    package nv renames Node_Vectors;

    type Heap is tagged limited record
        value : nv.Vector := nv.Empty_Vector;
    end record;

    function Left (idx : in Natural) return Natural;
    function Right (idx : in Natural) return Natural;
    function Parent (idx : in Natural) return Integer;

    procedure Bubble_Up (self : in out Heap; idx : in Natural);
    procedure Trickle_Down (self : in out Heap; idx : in Natural);


end Heaps;