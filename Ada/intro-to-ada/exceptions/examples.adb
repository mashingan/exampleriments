with Exceptions; use Exceptions;

procedure examples is
begin
    raise My_except;
    raise My_except with "My exception message";
end;
