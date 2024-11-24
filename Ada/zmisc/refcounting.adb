package refcounting is
   type Refcounted is abstract tagged private;

   procedure Free (self: in out Refcounted) is null;

   type Refcounted_Access is access all Refcounted'Class;
   type Ref is tagged private;

   procedure Set (self: in out Ref; Data: Refcounted'Class);
   function Get (self: Ref) return Refcounted_Access;
   procedure Finalize (P : in out Ref);
   procedure Adjust (P : in out Ref);

   private

   type Refcounted is abstract tagged record
      Refcount: Integer := 0;
   end record;

   type Ref is new Ada.Finalization.Controlled with record
      Data: Refcounted_Access;
   end record;
end refcounting;

package body refcounting is
   procedure Set (self: in out Ref; Data: Refcounted'Class) is
      D : constant Refcounted_Access := new Refcounted'Class(Data);
   begin
      if self.Data /= null then
         Finalize (self); -- decrement old reference count
      end if;

      self.Data := D;
      Adjust (self); -- increment reference count
   end Set;

   function Get (P : Ref) return Refcounted_Access is (P.Data);

   overriding procedure Adjust (P : in out Ref) is
   begin
      if P.Data /= null then
         P.Data.Refcount := P.Data.Refcount + 1;
      end if;
   end Adjust;

   overriding
   procedure Finalize (P : in out Ref) is
      Data: Refcounted_Access := P.Data;
   begin
      -- so next call to finalize will have no effect;
      P.Data := null;

      if Data /= null then
         Data.Refcount := Data.Refcount - 1;
         if Data.Refcount = 0 then
            Free (Data.all); -- call to user-defined primitive
            Unchecked_Free (Data);
         end if;
      end if;
   end Finalize;

end refcounting;
