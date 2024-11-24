import java.util.*;

public class Brackets {
    static ArrayList<Character> stack = new ArrayList<>();
    static final String openBrackets = "({[";
    static final String closeBrackets = ")}]";
    public static void main(String args[]) {
        var s = "(){}[]";
        for (var c : s.toCharArray()) {
            var ch = new Character(c);
            var openidx = openBrackets.indexOf(c);
            var closeidx = closeBrackets.indexOf(c);
            if (openidx > -1) {
                stack.add(ch);
            } else if (closeidx > -1) {
                if (stack.size() <= 0) {
                    System.out.println("stack 0");
                    return;
                }
                var lastIdx = stack.size() - 1;
                var cc = stack.get(lastIdx);
                stack.remove(lastIdx);
                var idx = openBrackets.indexOf(cc.charValue());
                if (closeBrackets.charAt(idx) != c) {
                    System.out.println("not valid!");
                    return;
                }
            }
        }
        System.out.println("valid!");
    }
}
