package main

import "fmt"

type LengthCounter func(string) int

func totalLengthDoubled(named, anon LengthCounter, sss ...string) int {
	result := 0
	for _, s := range sss {
		result += named(s)
		result += anon(s)
	}
	return result
}

func llen(s string) int { return len(s) }

func main() {
	tots := totalLengthDoubled(llen, func(s string) int { return len(s) },
		"aaa", "bbb", "ccc")
	fmt.Println(tots)
}
