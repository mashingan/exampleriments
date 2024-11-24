import std.stdio: writeln;

void main() {
	int b = 7_000_000;
	short c = cast(short) b;
	uint d = b;
	int g;
	assert(g == 0);

	auto f = 3.1415f;

	writeln("type of f is ", typeid(f));
	double pi = f;
	float demoted = pi;

	assert(int.init == 0);
	assert(int.sizeof == 4);
	assert(bool.max == 1);
	writeln(int.min, " ", int.max);
	writeln(int.stringof);
}
