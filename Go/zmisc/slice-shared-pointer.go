package main

import (
	"fmt"
	"sort"
)

func main() {
	s := []byte("randomdcbanice")
	const (
		start = 6
		end   = 10
	)
	fmt.Println(string(s))
	ss := []byte(s[start:end])
	fmt.Println(string(ss))
	sort.Slice(ss, func(i int, j int) bool {
		return ss[i] < ss[j]
	})
	fmt.Println(string(s))
	fmt.Println(string(ss))
}
