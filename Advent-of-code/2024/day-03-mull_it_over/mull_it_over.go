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
	const (
		rfmt = `mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)`
		// f, err := os.Open("sample.txt")
		do   = "do()"
		dont = "don't()"
	)
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()
	mulexpr := regexp.MustCompile(rfmt)
	scanner := bufio.NewScanner(f)
	sumPart1 := 0
	sumPart2 := 0
	enabled := true
	for scanner.Scan() {
		txt := scanner.Text()
		// log.Println(txt)
		var l, r int
		for _, s := range mulexpr.FindAllString(txt, -1) {
			log.Println(s)
			if s == do {
				enabled = true
				continue
			} else if s == dont {
				enabled = false
				continue
			}
			if n, err := fmt.Sscanf(s, "mul(%d,%d)", &l, &r); err != nil {
				log.Println(err)
			} else if n != 2 {
				log.Println("scanned not match, expected 2 got:", n)
			} else {
				sumPart1 += l * r
				if enabled {
					sumPart2 += l * r
				}
			}
		}
	}
	fmt.Printf("%10d\n", sumPart1)
	fmt.Printf("%10d\n", sumPart2)
}
