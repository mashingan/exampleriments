package main

import (
	"fmt"
	"os"
	"time"
)

func main() {
	const testCnt = 100
	const concatCnt = 1000
	s1 := "abcdefghijkl"
	expectSize := len(s1) * concatCnt

	t0 := time.Now()
	for i := 0; i < testCnt; i++ {
		var out string
		for j := 0; j < concatCnt; j++ {
			out += s1
		}
		if len(out) != expectSize {
			fmt.Println("Err: invalid length", len(out))
			os.Exit(1)
		}
	}
	d := time.Now().Sub(t0)
	fmt.Println("Normal +:", d)

	t0 = time.Now()
	for i := 0; i < testCnt; i++ {
		var s []byte
		for j := 0; j < concatCnt; j++ {
			s = append(s, s1...)
		}
		out := string(s[:])
		if len(out) != expectSize {
			fmt.Println("Err: invalid length", len(out))
			os.Exit(1)
		}
	}
	d = time.Now().Sub(t0)
	fmt.Println("append by []byte:", d)

	t0 = time.Now()
	for i := 0; i < testCnt; i++ {
		s := make([]byte, 0, expectSize)
		for j := 0; j < concatCnt; j++ {
			s = append(s, s1...)
		}
		out := string(s[:])
		if len(out) != expectSize {
			fmt.Println("Err: invalid length", len(out))
			os.Exit(1)
		}
	}
	d = time.Now().Sub(t0)
	fmt.Println("append by pre-allocated []byte:", d)
}
