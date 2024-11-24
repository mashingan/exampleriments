package main

import (
	"fmt"
	"strconv"
)

func calc(s []string) int {
	result := 0
	nums := []int{}
	for _, ss := range s {
		n, err := strconv.Atoi(ss)
		if err == nil {
			nums = append(nums, n)
		} else {
			lastIdx := len(nums) - 1
			switch ss {
			case "+":
				newnum := nums[lastIdx] + nums[lastIdx-1]
				nums = append(nums, newnum)
			case "D":
				prevnum := nums[lastIdx]
				nums = append(nums, 2*prevnum)
			case "C":
				nums = nums[:lastIdx]
			}
		}
	}
	for _, n := range nums {
		result += n
	}
	return result
}

func main() {
	input := []string{"5", "2", "C", "D", "+"}
	fmt.Println(calc(input))
	input = []string{"5", "-2", "4", "C", "D", "9", "+", "+"}
	fmt.Println(calc(input))
	input = []string{"1"}
	fmt.Println(calc(input))
}
