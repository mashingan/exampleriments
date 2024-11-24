package main

import (
	"bufio"
	_ "fmt"
	"os"
	"regexp"
)

func main() {
	var s string
	var err error
	/*
		n, err := fmt.Scanf("%s", &s)
		if err != nil {
			panic(err)
		}
		fmt.Println(n)
		fmt.Println(s)

	*/
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		s = scanner.Text()
		break
	}
	if err = scanner.Err(); err != nil {
		panic(err)
	}
	_ = regexp.MustCompile(s)
	if err != nil {
		panic(err)
	}
}
