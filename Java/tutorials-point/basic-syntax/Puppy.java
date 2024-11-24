public class Puppy {
	String name;
	public Puppy(String name) {
		this.name = name;
		System.out.println("Passed name is: " + this.name);
	}

	public static void main(String []args) {
		String puppyname;
		if (args.length > 0) {
			puppyname = args[0];
		} else {
			puppyname = "Tommy";
		}
		var mypuppy = new Puppy(puppyname);
	}
}
