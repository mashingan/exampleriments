class FreshJuice {
	enum FreshJuiceSize { Small, Medium, Large }
	FreshJuiceSize size;
}

public class FreshJuiceTest {
	public static void main(String []args) {
		var juice = new FreshJuice();
		juice.size = FreshJuice.FreshJuiceSize.Medium;
		System.out.println("Size: " + juice.size);
	}
}
