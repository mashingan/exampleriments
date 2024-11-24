package main

import (
	"fmt"
	"regexp"
)

func checkIt(reg *regexp.Regexp, str string) {
	if !reg.MatchString(str) {
		fmt.Printf("'%s' doesn't contain 'no-store'\n", str)
	} else {
		fmt.Printf("'%s' contains 'no-store'\n", str)
	}
}

func main() {
	noStore := regexp.MustCompile(`^.*(no-store).*`)
	str1 := "Cache-Control: private, no-cache, must-revalidate"
	str2 := "Cache-Control: private, no-cache, no-store, must-revalidate"
	checkIt(noStore, str1)
	checkIt(noStore, str2)
}
