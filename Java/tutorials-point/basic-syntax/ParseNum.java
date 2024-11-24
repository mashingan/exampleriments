public class ParseNum {
	public static void main(String []args) {
		var fivestr = "5";
		int num = 0;
		try {
			num = Integer.parseInt(fivestr);
		} catch (NumberFormatException e) {
			System.out.println(e);
		}
		System.out.println("The num is: " + num);

		num = 0;
		try {
			num = Integer.parseInt("invalid string");
		} catch (NumberFormatException e) {
			System.out.println(e);
		}
		System.out.println("The num after invalid is: " + num);
	}
}
