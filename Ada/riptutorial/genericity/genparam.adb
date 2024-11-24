generic
type Component is private;
type Index is (<>);
with function "<"(Left, Right: Component) return Boolean;
procedure Sort (A: in out Array_Type);
