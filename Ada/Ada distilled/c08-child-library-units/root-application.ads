package root.application is
	type Application_Type is private;
	procedure Create(A: in out Application_Type);
	function Is_empty(A: Application_Type) return Boolean;

	private;
	type Application_Type is record
	end record;
end root.application;
