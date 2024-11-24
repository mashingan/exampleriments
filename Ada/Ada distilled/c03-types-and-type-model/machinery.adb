package body Machinery is
	procedure Turn_on(M: in out Machine) is
	begin
		M.Turned_on := true;
	end Turn_on;

	procedure Turn_off(M: in out Machine) is
	begin
		M.Turned_on := false;
	end Turn_off;

	function Is_on(M: in Machine) return Boolean is
	begin
		return M.Turned_on;
	end Is_on;

	function ">"(L, R: Machine) return Boolean is
	begin
		return L.Turned_on or R.Turned_on;
	end ">"
end Machinery;
