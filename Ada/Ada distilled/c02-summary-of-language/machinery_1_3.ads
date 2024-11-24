package Machinery_1_3 is
	type Machine is private;
	procedure Turn_On (M: in out Machine);
	procedure Turn_Off (M: in out Machine);
	function Is_on(M: in Machine) return Boolean;
private
	type Machine is record
		Turned_on: Boolean := false;
	end record;
end Machinery_1_3;
