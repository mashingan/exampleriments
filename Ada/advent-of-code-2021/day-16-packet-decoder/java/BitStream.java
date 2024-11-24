import java.util.BitSet;

public class BitStream {
	BitSet bits;
	int pos;

	BitStream() {
		bits = new BitSet();
		pos = 0;
	}

	void add(boolean set) {
		bits.set(pos++, set);
	}

	boolean get(int index) {
		return bits.get(index);
	}

	boolean get() {
		return bits.get(pos);
	}

	boolean getAndIncrement() {
		return bits.get(pos++);
	}

	void setPos(int pos) {
		this.pos = pos;
	}

	void set(int index) {
		bits.set(index);
	}

	void clear(int index) {
		bits.clear(index);
	}

	void set(int index, boolean set) {
		bits.set(index, set);
	}

	int toInt() {
		var nbits = new BitSet();
		for (var i = 0; i < bits.size(); i++) {
			nbits.set(bits.size()-1-i, bits.get(i));
		}
		var lngs = nbits.toLongArray();
		if (lngs.length > 0) {
			return (int) lngs[0];
		}
		return 0;
	}

	void checkBytes(byte[] bytes) {
		for (var b : bytes) {
			System.out.print(b);
		}
		System.out.println();
	}

	int toInt(int start, int end) {
		//System.out.printf("ori: %s, with (start, end): (%d, %d)\n", bits.get(start, end).toString(), start, end);
		var nbits = new BitSet();
		for (var i = end-start-1; i >= 0; i--) {
			nbits.set(i, bits.get(end-1-i));
		}
		var lngs = nbits.toLongArray();
		if (lngs.length > 0) {
			return (int) lngs[0];
		}
		return 0;
	}

	public String toString() {
		var sb = new StringBuilder();
		for (var i = 0; i < bits.size(); i++) {
			if (bits.get(i)) {
				sb.append('1');
			} else {
				sb.append('0');
			}
		}
		return sb.toString();
	}
}
