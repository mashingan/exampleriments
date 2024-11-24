// build with:
// go build -o a.exe big-struct-arg-pass.go
// dump the info with
// go tool objdump -s main.main a.exe

package main

type Bigs struct {
	Field1  int
	Field2  string
	Field3  float32
	Field4  float64
	Bytes   []byte
	PtrStr  *string
	Field12 int
	Field22 string
	Field32 float32
	Field42 float64
	Bytes2  []byte
	PtrStr2 *string
	Field13 int
	Field23 string
	Field33 float32
	Field43 float64
	Bytes3  []byte
	PtrStr3 *string
	//When time.Time
}

func Donothing(b Bigs) {
	for i := 0; i < 10; i++ {
		b.Field1 += i
	}
}

func PtrDonothing(b *Bigs) {
	for i := 0; i < 10; i++ {
		b.Field1 += i
	}
}

func main() {
	s := "hello"
	b := Bigs{
		Field1:  1,
		Field2:  "wan",
		Field3:  1.1,
		Field4:  1.2,
		Bytes:   []byte("wanwan"),
		PtrStr:  &s,
		Field12: 1,
		Field22: "wan",
		Field32: 1.1,
		Field42: 1.2,
		Bytes2:  []byte("wanwan"),
		PtrStr2: &s,
		Field13: 1,
		Field23: "wan",
		Field33: 1.1,
		Field43: 1.2,
		Bytes3:  []byte("wanwan"),
		PtrStr3: &s,
		//When: time.Now(),
	}
	Donothing(b)
	PtrDonothing(&b)
}
