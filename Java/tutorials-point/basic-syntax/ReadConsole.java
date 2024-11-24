import java.io.InputStreamReader;

public class ReadConsole {
	public static void main(String []args) {
		InputStreamReader in = null;
		try {
			in = new InputStreamReader(System.in);
			System.out.println("Enter characters, q to quit.");
			char c;
			do {
				c = (char) in.read();
				System.out.print(c);
			} while(c != 'q');
		} catch(Exception e) {
		} finally {
			if (in != null) {
				try { in.close(); } catch(Exception e) {}
			}
		}
	}
}
