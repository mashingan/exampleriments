package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"slices"
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
		// log.Println(txt)
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
	// log.Println(updates)
	// log.Println(pageOrder)
	summid := 0
	summUpdate := 0
	for _, up := range updates {
		// log.Println("====updates:", up, "====")
		validUpdate, correctedUpdate := checkOrdering(up, pageOrder)
		if validUpdate {
			mid := up[len(up)/2]
			summid += mid
		} else {
			// log.Println("==incorrect update:", up, "==")
			// log.Println("==correction update:", correctedUpdate, "==")
			invalidOrdering := !validUpdate
			for invalidOrdering {
				validUpdate, correctedUpdate = checkOrdering(correctedUpdate, pageOrder)
				invalidOrdering = !validUpdate
			}
			summUpdate += correctedUpdate[len(correctedUpdate)/2]
		}
		// log.Println("====end updates====")

	}
	fmt.Println(summid)
	fmt.Println(summUpdate)
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

func checkOrdering(up []int, pageOrder map[int][]int) (bool, []int) {
	validUpdate := true
	updateCorrections := slices.Clone(up)
	updateOrderPos := map[int]int{}
	for i, n := range up {
		updateOrderPos[n] = i
	}
	for i := 0; i < len(up); i++ {
		order := up[i]
		// log.Println("--order:", order, "--")
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
			for _, n := range mustNotBefore {
				pos := updateOrderPos[n]
				// log.Println("i:", i, ", pos:", pos)
				updateCorrections[i], updateCorrections[pos] = updateCorrections[pos], updateCorrections[i]
				// log.Println("current correction:", updateCorrections)
			}
		}
		// log.Println("order:", order)
		// log.Println("mustNotBefore:", before)
		// log.Println("mustAfter:", mustAfter)
		// log.Println("ordering:", ordering)
		// log.Println("orders:", orders)
		// log.Println("mustNotBefore:", mustNotBefore)
	}
	return validUpdate, updateCorrections
}
