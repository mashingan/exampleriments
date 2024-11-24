import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Stack;

public class Day10SyntaxScoring {
	public static void main(String []args) {
		HashMap<Character, Integer> pointsMap = HashMap.newHashMap(4);
		pointsMap.put(')', 3);
		pointsMap.put(']', 57);
		pointsMap.put('}', 1197);
		pointsMap.put('>', 25137);
		HashMap<Character, Character> bracketsPair = HashMap.newHashMap(4);
		bracketsPair.put('(', ')');
		bracketsPair.put('[', ']');
		bracketsPair.put('{', '}');
		bracketsPair.put('<', '>');
		var bracketsOpen = bracketsPair.keySet();
		var bracketsClose = pointsMap.keySet();
		var buf = new BufferedReader(new InputStreamReader(System.in));
		var stack = new Stack<Character>();
		int total = 0;
		try {
			while (true) {
				var s = buf.readLine();
				if (s == null || s.isEmpty()) { break; }
				for (var c : s.toCharArray()) {
					if (bracketsOpen.contains(c)) {
						stack.push(bracketsPair.get(c));
					} else if (bracketsClose.contains(c)) {
						var cc = stack.pop();
						if (cc != c) {
							total += pointsMap.get(c);
						}
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		System.out.println("The total points of mismatched syntax is " + total);
	}
}
