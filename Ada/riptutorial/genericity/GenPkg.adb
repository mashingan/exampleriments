generic
    type T is private;

package GenPkg is
    type C is tagged record
        V: T;
end record;

G: Integer;

end GenPkg;
