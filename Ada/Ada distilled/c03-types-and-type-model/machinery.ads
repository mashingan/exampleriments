package Machinery is
	type Machine is private;
	procedure Turn_on(M: in out Machine);
	procedure Turn_off(M: in out Machine);
	function Is_on(M: in Machine) return Boolean;
	function ">"(L, R: Machine) return Boolean;

	private
	type Machine is record
		Turned_on : Boolean := false;
	end record;
end Machinery;
