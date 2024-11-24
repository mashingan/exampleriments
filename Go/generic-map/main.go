package main

import "fmt"

func Map[T any](arr []T, fn func(T) T) (result []T) {
	result = make([]T, len(arr))
	for i, t := range arr {
		result[i] = fn(t)
	}
	return
}

func Filter[T any](arr []T, fn func(T) bool) (result []T) {
	for _, t := range arr {
		if fn(t) {
			result = append(result, t)
		}
	}
	return
}

func main() {
	one2five := []int{1, 2, 3, 4, 5}
	fmt.Println("map double of one to five",
		Map(one2five, func(a int) int { return a * 2 }))
	fmt.Println("filter even of one to five",
		Filter(one2five, func(a int) bool { return a%2 == 0 }))
}
