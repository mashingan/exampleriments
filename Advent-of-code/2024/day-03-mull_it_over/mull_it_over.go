package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	const rfmt = `mul\(\d{1,3},\d{1,3}\)`
	// f, err := os.Open("sample.txt")
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()
	mulexpr := regexp.MustCompile(rfmt)
	scanner := bufio.NewScanner(f)
	sum := 0
	for scanner.Scan() {
		txt := scanner.Text()
		// log.Println(txt)
		var l, r int
		for _, s := range mulexpr.FindAllString(txt, -1) {
			log.Println(s)
			if n, err := fmt.Sscanf(s, "mul(%d,%d)", &l, &r); err != nil {
				log.Println(err)
			} else if n != 2 {
				log.Println("scanned not match, expected 2 got:", n)
			} else {
				sum += l * r
			}
		}
	}
	fmt.Println(sum)
}
