package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"sort"
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	input, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer input.Close()
	// 18944   47230
	scanner := bufio.NewScanner(input)
	left := []int{}
	right := []int{}
	m := make(map[int]int)
	for scanner.Scan() {
		txt := scanner.Text()
		var l, r int
		fmt.Sscanf(txt, "%d   %d", &l, &r)
		occurrence := m[r]
		m[r] = occurrence + 1
		left = append(left, l)
		right = append(right, r)
		sort.Ints(left)
		sort.Ints(right)
	}
	diff := 0
	simi := 0
	for i, l := range left {
		simi += l * m[l]
		diff += int(math.Abs(float64(l - right[i])))
	}
	fmt.Println(diff)
	fmt.Println(simi)
}
