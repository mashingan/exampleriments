package body Machinery_1_3 is
	procedure Turn_on (M: in out Machine) is
	begin
		M.turned_on := true;
	end Turn_on;

	procedure Turn_off (M: in out Machine) is
	begin
		M.turned_on := false;
	end Turn_off;

	function Is_on(M: in Machine) return Boolean is
	begin
		return M.turned_on;
	end Is_on;
end Machinery_1_3;
