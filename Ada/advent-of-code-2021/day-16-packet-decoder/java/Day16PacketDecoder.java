import java.io.BufferedReader;
import java.io.InputStreamReader;

class Literal {
	boolean last;
	int pos;
	int value;
}

public class Day16PacketDecoder {
	static final int PacketLiteral = 4;

	public static void main(String[] args) {
		var buf = new BufferedReader(new InputStreamReader(System.in));
		var totalVersion = 0;
		try {
			while(true) {
				var bits = new BitStream();
				var s = buf.readLine();
				if (s == null || s.isEmpty()) break;
				System.out.println("readLine: " + s);
				for (var i = 0; i < s.length(); i++) {
					var c = s.charAt(i);
					var cnum = Integer.parseInt(c+"", 16);
					var sbin = Integer.toBinaryString(cnum);
					if (sbin.length() < 4) {
						sbin = "0".repeat(4-sbin.length()) + sbin;
					}
					//System.out.printf("c: %c, cnum: %2d, sbin: %4s\n", c, cnum, sbin);
					for (var j = 0; j < sbin.length() && j < 4; j++) {
						if (sbin.charAt(j) == '1') {
							bits.add(true);
						} else {
							bits.add(false);
						}
					}
				}
				bits.setPos(0);
				var thistotal = decodePacket(bits);
				System.out.println("the packet size is " + bits.bits.size() +
						", current total: " + thistotal);
				totalVersion += thistotal;
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		System.out.println("The total version from packet is " + totalVersion);
	}

	static Literal decodeLiterals(BitStream bits) {
		var res = new Literal();
		res.last = !bits.getAndIncrement();
		res.value = bits.toInt(bits.pos, bits.pos+4);
		bits.setPos(bits.pos+4);
		res.pos = bits.pos;
		return res;
	}

	static int decodePacketLiterals(BitStream bits) {
		Literal res = new Literal();
		do {
			res = decodeLiterals(bits);
		} while(!res.last);
		return bits.pos;
	}

	static int decodeOperator1(BitStream bits) {
		var total = 0;
		var subpacks = bits.toInt(bits.pos, bits.pos+11);
		bits.setPos(bits.pos + 11);
		for (var i = 1; i <= subpacks; i++) {
			total += decodePacket(bits);
		}
		return total;
	}

	static int decodeOperator0(BitStream bits) {
		var total = 0;
		var bitlen = bits.toInt(bits.pos, bits.pos+15);
		bits.setPos(bits.pos+15);
		var prevpos = bits.pos;
		var count = 0;
		while (count < bitlen) {
			total += decodePacket(bits);
			count += bits.pos - prevpos;
			prevpos = bits.pos;
		}
		return total;
	}

	static int decodePacket(BitStream bits) {
		var version = bits.toInt(bits.pos, bits.pos+3);
		var type = bits.toInt(bits.pos+3, bits.pos+6);
		bits.setPos(bits.pos+6);
		if (type == PacketLiteral) {
			decodePacketLiterals(bits);
			return version;
		} else {
			if (bits.getAndIncrement()) {
				return version + decodeOperator1(bits);
			} else {
				return version + decodeOperator0(bits);
			}
		}
	}
}
