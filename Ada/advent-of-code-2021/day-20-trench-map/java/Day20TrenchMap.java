import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.BitSet;

public class Day20TrenchMap {
	public static void main(String[] args) {
		var buf = new BufferedReader(new InputStreamReader(System.in));
		var image = new ArrayList<String>();
		var imageEnhancementAlgo = "";
		try {
			imageEnhancementAlgo = buf.readLine();
			buf.readLine();

			while(true) {
				var s = buf.readLine();
				if (s == null || s.isEmpty()) break;
				image.add('.' + s + '.');
			}
		} catch(Exception e) {
			e.printStackTrace();
		}
		var lenstr = image.get(0).length();
		var paddingDots = ".".repeat(lenstr);
		image.add(0, paddingDots);
		image.add(paddingDots);
		render(image);
		for (var i = 0; i < 2; i++) {
			System.out.println("Enhancement: " + (i+1));
			image = enhance(imageEnhancementAlgo, image);
			render(image);
		}
		var total = 0;
		for (var s : image.toArray(String[]::new)) {
			for (var c : s.toCharArray()) {
				if (c == '#') total++;
			}
		}
		System.out.println("Total marked dots after 2 enhancements is " + total);
	}

	static boolean dot(char c) { return c == '#'; }
	static ArrayList<String> enhance(String imageEnhancementAlgo, ArrayList<String> image) {
		var newimage = new ArrayList<String>();
		for (var i = 0; i < image.size(); i++) {
			var s = image.get(i);
			var buf = ".";
			for (var j = 0; j < s.length(); j++) {
				var bs = new BitSet();
				var pos = 0;
				var ss = "";
				if (i - 1 < 0) {
					ss = ".".repeat(s.length()+2);
				} else {
					ss = '.' + image.get(i-1) + '.';
				}
				for (var k = j; k <= j+2; k++) {
					bs.set(pos++, dot(ss.charAt(k)));
				}

				ss = '.' + s + 's';
				for (var k = j; k <= j+2; k++) {
					bs.set(pos++, dot(ss.charAt(k)));
				}

				if (i + 1 < s.length()) {
					ss = '.' + image.get(i+1) + '.';
				} else {
					ss = ".".repeat(s.length()+2);
				}
				for (var k = j; k <= j+2; k++) {
					bs.set(pos++, dot(ss.charAt(k)));
				}
				var num = toInt(bs);
				buf += imageEnhancementAlgo.charAt(num);
			}
			buf += '.';
			newimage.add(buf);
		}
		var linelen = newimage.get(0).length();
		var padding = ".".repeat(linelen);
		newimage.add(0, padding);
		newimage.add(padding);
		return newimage;
	}

	static void render(ArrayList<String> image) {
		for (var s : image.toArray(String[]::new)) {
			System.out.println(s);
		}
		System.out.println();
	}

	static void checkbits(BitSet bs) {
		for (var i = 0; i < bs.size(); i++) {
			System.out.printf("%c", bs.get(i) ? '1' : '0');
		}
		System.out.println();
	}

	static int toInt(BitSet bs) {
		var nbs = new BitSet();
		for (var i = 0; i < 9; i++) {
			nbs.set(i, bs.get(9-1-i));
		}
		var lngs = nbs.toLongArray();
		//var lngs = bs.toLongArray();
		if (lngs.length > 0) {
			return (int) lngs[0];
		}
		return 0;
	}
}
