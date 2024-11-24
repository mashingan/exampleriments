package abstract_machinery.classwide is
	type FIFO_container(Size: positive) is limited private;
	procedure Put(CM: in out FIFO_container, Data: access Machine'class);

	private
	type Machine_Data is array (Positive range <>) of Reference;
	type FIFO_container(Size: positive) is record
		Current: Natural;
		Data : Machine_Data(1..Size);
	end record;
end abstract_machinery.classwide;
