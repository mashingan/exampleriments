import java.awt.Point;

public class Distance {
    private Point p0, p1;

    public Distance(int x0, int y0, int x1, int y1) {
        p0 = new Point(x0, y0);
        p1 = new Point(x1, y1);
    }

    public void printDistance() {
        System.out.println("Distance between " + p0 + " and " + p1 +
                " is " + p0.distance(p1));
    }

    public static void main(String[] args) {
        Distance dist = new Distance(
                intValue(args[0]), intValue(args[1]),
                intValue(args[2]), intValue(args[3]));
        dist.printDistance();
    }

    private static int intValue(String data) {
        return Integer.parseInt(data);
    }
}
