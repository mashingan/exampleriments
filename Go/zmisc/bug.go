package main

var x int32 = -1

func main() {
	if x != -1 {
		panic("oh no")
	}

	if x > 0 || x != -1 {
		panic("oh no #golang")
	}
}
