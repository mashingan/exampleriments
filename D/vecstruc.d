import std.stdio: writeln;
import std.string: format;

struct Vector3 {
	double x;
	double y;
	double z;

	double length() const {
		import std.math: sqrt;
		return sqrt((x*x) + (y*y) + (z*z));
	}

	double dot(Vector3 rhs) const {
		return (x*rhs.x) + (y*rhs.y) + (z*rhs.z);
	}

	string toString() {
		return format("Vector3(%.3f, %.3f, %.3f)", x, y, z);
	}
}

void main() {
	auto v1 = Vector3(10, 0, 0);
	writeln("v1:", v1);
	Vector3 v2;
	v2.x = 0;
	v2.y = 20;
	v2.z = 0;
	writeln("v2:", v2);

	assert(v1.length == 10);
	assert(v2.length == 20);
	assert(v1.dot(v2) == 0);

	auto v3 = Vector3(1, 2, 3);
	writeln("v3:", v3);
	assert(v3.dot(Vector3(1, 1, 1)) == 6);
	assert(v3.dot(Vector3(3, 2, 1)) == 10);
}
