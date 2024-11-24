import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

public class CopyFile {
	public static void main(String []args) {
		FileInputStream in = null;
		FileOutputStream out = null;

		try {
			in = new FileInputStream("input.txt");
			out = new FileOutputStream("output.txt");

			int c;
			while ((c = in.read()) != -1) {
				out.write(c);
			}
		} catch (IOException e) {
			System.out.println(e);
		} finally {
			if (in != null) {
				try {
					in.close();
				} catch(Exception e) {
				}
			}
			if (out != null) {
			try {
				out.close();
			} catch(Exception e) {
			}
			}
		}
	}
}
