package abstract_machinery is
	type Machine is abstract tagged private;
	type Reference is access all Machine'class;
	function Create(Desc: String) return Machine'class;
	procedure Turn_on(M: in out Machine);
	procedure Turn_off(M: in out Machine);
	function Is_on(M: in Machine) return Boolean;

	private
	type Machine is abstract tagged record
		Turned_on : Boolean := false;
		Description : String(1..120);
	end record;
end abstract_machinery;
