package main

import (
	"aoc-2024/common"
	"log"
	"strconv"
	"strings"
)

const (
	// input = "input.txt"
	input = "sample.txt"
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	testvalues := map[int][]int{}
	common.ReadLines(input, func(text string) {
		nums := strings.Split(text, " ")
		vals := make([]int, len(nums)-1)
		n, _ := strconv.Atoi(strings.Trim(nums[0], ":"))
		vals[0] = n
		for i := 1; i < len(nums); i++ {
			n, _ = strconv.Atoi(strings.TrimSpace(nums[i]))
			vals[i] = n
		}
		testvalues[vals[0]] = vals[1:]
	})
}
