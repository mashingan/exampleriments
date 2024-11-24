with Ada.Integer_Text_IO;
with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Bounded;
with Ada.Containers;

package body Snailfish_Trees is

	function "="(t1, t2: in Tree_element) return Boolean is
		(t1.kind = Leaf and t2.kind = Leaf and (t1.Num = t2.Num));

	function To_String(c: in out sft.Cursor) return String is
		use Max120String;
		use sft;
		use Ada.Containers;
		buf : Bounded_String;
		elm : Tree_element;
	begin
		if c = No_Element then return ""; end if;
		elm := sft.Element(c);
		if elm.kind = Leaf then
			return elm.Num'image;
		else
			buf := buf & '[';
			c := sft.First_Child(c);
			buf := buf & To_String(c) & ',';
			sft.Next_Sibling(c);
			buf := buf & To_String(c) & ']';
			c := sft.Parent(c);
			return To_String(buf);
		end if;
	end To_String;

	function To_String(tree: in sft.Tree) return String is
		use Max120String;
		cur : sft.Cursor := sft.Root(tree);
		str1 : Bounded_String;
	begin
		cur := sft.First_Child(cur);
		str1 := str1 & To_String(cur);
		sft.Next_Sibling(cur);
		return '[' & To_String(str1) & ',' & To_String(cur) & ']';
	end To_String;

	procedure Read_Fish(s: in String; last: in out Integer;
		tree: in out sft.Tree;
		cur: in out sft.Cursor)
	is
		use sft;
		package nio renames Ada.Integer_Text_IO;
		num : Integer;
	begin
		if cur = No_Element then
			cur := root(tree);
		end if;
		if last <= s'last then
			if s(last) = '[' then
				last := last + 1;
				tree.Append_Child(cur, (Kind => Branch));
				cur := First_Child(cur);
				Read_Pair_Fish(s, last, tree, cur);
				if last <= s'last then
					if s(last) = ']' then last := last + 1; end if;
				end if;
			else
				nio.Get(s(last..s'last), num, last);
				last := last + 1;
				tree.Append_Child(cur, (Leaf, num));
			end if;
		end if;
	end Read_Fish;

	procedure Read_Pair_Fish(s: in String; last: in out Integer;
		tree: in out sft.Tree; cur: in out sft.Cursor)
	is
	begin
		if last <= s'last then
			Read_Fish(s, last, tree, cur);
			if s(last) /= ',' then
				last := last + 1;
				tree.Append_Child(cur, (Leaf, 0));
			else
				last := last + 1;
				Read_Fish(s, last, tree, cur);
				if last <= s'last then
					if s(last) = ']' then last := last + 1; end if;
				end if;
			end if; 
		end if;
	end Read_Pair_Fish;

	function Read_Snailfish(s: in String) return sft.Tree is
		fishes : sft.Tree := sft.Empty_Tree;
		cur : sft.Cursor;
		last : Positive := 1;
	begin
		cur := sft.root(fishes);
		Read_Fish(s, last, fishes, cur);
		return fishes;
	end Read_Snailfish;

	procedure Read_Snailfishes is
		line : String(1..120);
		length: Natural;
		fishes : sft.Tree := sft.Empty_Tree;
		cur : sft.Cursor := sft.No_Element;
		last : Positive := 1;
	begin
		loop
			begin
				tio.Get_Line(line, length);
			exception
				when tio.Data_Error => exit;
				when tio.End_Error => exit;
			end;
			exit when length = 0;
			cur := sft.Root(fishes);
			Read_Fish(line(line'first .. length), last, fishes, cur);
			Snailfishes.Append(fishes);
			fishes.clear;
		end loop;
	end Read_Snailfishes;
end Snailfish_Trees;
