package main

import (
	"fmt"
	"strings"
)

func check(s string) bool {
	const (
		openBrackets  = "({["
		closeBrackets = ")}]"
	)
	stack := []byte{}
	for _, c := range s {
		openIdx := strings.IndexByte(openBrackets, byte(c))
		/*
			fmt.Println("openIdx:", openIdx)
			fmt.Println("c:", c)
			fmt.Println("byte(c):", byte(c))
		*/
		//closeIdx := strings.IndexByte(s, byte(c))
		if openIdx > -1 {
			stack = append(stack, byte(c))
		} else {
			slen := len(stack)
			if slen == 0 {
				return false
			}
			cc := stack[slen-1]
			stack = stack[:slen]
			//fmt.Println(stack)
			idx := strings.IndexByte(openBrackets, cc)
			if closeBrackets[idx] != byte(c) {
				return false
			}
		}
	}
	return true
}

func main() {
	s := "(){}[]"
	fmt.Println(check(s))
	s = "([)]"
	fmt.Println(check(s))
}
