public class EmployeeTest {
	public static void main(String []args) {
		var emp1 = new Employee("James Smith");
		var emp2 = new Employee("Mary Sue");
		emp1.age(26);
		emp1.designation("senior software engineer");
		emp1.salary(1000);
		emp1.check();

		emp2.age(21);
		emp2.designation("software engineer");
		emp2.salary(500);
		emp2.check();
	}
}
