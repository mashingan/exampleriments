package main

import "fmt"

func main() {
	ochan := make(chan string)
	go func(oc chan string) {
		for i := 0; i < 5; i++ {
			oc <- fmt.Sprintf("msg-%02d", i)
		}
		oc <- "quit"
	}(ochan)

	for msg := range ochan {
		if msg == "quit" {
			break
		}
		fmt.Println(msg)
	}
}
