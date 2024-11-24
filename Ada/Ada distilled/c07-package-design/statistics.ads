package statistics is
	type Data is array (Positive range <>) of Float;
	function Mean (The_Data: Data) return Float;
	function Mode (The_Data: Data) return Float;
	function Max  (The_Data: Data) return Float;
	function Min  (The_Data: Data) return Float;
	function Variance (The_Data: Data) return Float;
	function StdDev (The_Data: Data) return Float;
end statistics;
