package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	// f, err := os.Open("sample.txt")
	f, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()
	scanner := bufio.NewScanner(f)
	pageOrderingDone := false
	pageOrder := make(map[int][]int)
	updates := [][]int{}
	for scanner.Scan() {
		txt := scanner.Text()
		if txt == "" {
			pageOrderingDone = true
			continue
		}
		if !pageOrderingDone {
			var l, r int
			fmt.Sscanf(txt, "%d|%d", &l, &r)
			prev, ok := pageOrder[l]
			if !ok {
				pageOrder[l] = []int{r}
				continue
			}
			prev = append(prev, r)
			pageOrder[l] = prev
			continue
		}
		ns := strings.Split(txt, ",")
		if len(ns) <= 0 {
			continue
		}
		ints := make([]int, len(ns))
		for i, s := range ns {
			n, _ := strconv.Atoi(s)
			ints[i] = n
		}
		updates = append(updates, ints)
	}
	summid := 0
	for _, up := range updates {
		validUpdate := true
		for i := 0; i < len(up); i++ {
			order := up[i]
			before := up[:i]
			mustAfter := []int{}
			if i+1 < len(up) {
				mustAfter = up[i+1:]
			}
			orders := pageOrder[order]
			ordering := intersection(orders, mustAfter)
			mustNotBefore := intersection(before, orders)
			if len(ordering) != len(mustAfter) || len(mustNotBefore) > 0 {
				validUpdate = false
			}
		}
		if validUpdate {
			mid := up[len(up)/2]
			summid += mid

		}

	}
	fmt.Println(summid)
}

func in[T comparable](e T, arr []T) bool {
	for _, a := range arr {
		if a == e {
			return true
		}
	}
	return false
}

func intersection[T comparable](a, b []T) []T {
	result := []T{}
	for _, e := range a {
		if in(e, b) {
			result = append(result, e)
		}
	}
	return result
}
