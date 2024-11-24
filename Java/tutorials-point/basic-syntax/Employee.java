public class Employee {
	String name;
	int age;
	String designation;
	double salary;

	public Employee(String name) {
		this.name = name;
	}

	public void age(int age) {
		this.age = age;
	}

	public int age() {
		return this.age;
	}

	public void designation(String designation) {
		this.designation = designation;
	}

	public String designation() {
		return this.designation;
	}

	public void salary(double salary) {
		this.salary = salary;
	}

	public double salary() {
		return this.salary;
	}

	public void check() {
		System.out.println("Name: " + name);
		System.out.println("Age: " + age);
		System.out.println("Designation: " + designation);
		System.out.println("Salary: " + salary);
	}
}
