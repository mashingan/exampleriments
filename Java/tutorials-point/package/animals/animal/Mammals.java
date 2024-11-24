package animal;

public class Mammals implements Animal {
	public void eat() {
		System.out.println("Mammals eat");
	}

	public void travel() {
		System.out.println("Mammals travel");
	}

	int legs = 0;
	public int legs() {
		return legs;
	}

	public static void main(String []args) {
		var m = new Mammals();
		m.eat();
		m.travel();
	}
}
