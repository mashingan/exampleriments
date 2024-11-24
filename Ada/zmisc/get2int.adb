with Ada.Text_IO;           use Ada.Text_IO; 
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;  

procedure get2int is   

procedure Two_Integers (Integer_1, Integer_2 : out Integer) is
begin              
	Put("Type two integers: ");
	Get(Integer_1);       
	Get(Integer_2);          
end Two_Integers;               

Integer_1: Integer;
Integer_2: Integer;

begin         
	Two_Integers(Integer_1, Integer_2);      
	Put("The two integers were:");      
	Put(Integer_1'image & " " & Integer_2'image);
end get2int;  
