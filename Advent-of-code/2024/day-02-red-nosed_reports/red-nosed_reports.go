package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"slices"
	"strconv"
	"strings"
)

type dampenerSubject struct {
	ns  []int
	pos int
}

func main() {
	log.SetFlags((log.LstdFlags | log.Lshortfile))
	f, err := os.Open("input.txt")
	// f, err := os.Open("sample.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()
	scanner := bufio.NewScanner(f)
	countLevelSafe := 0
	ds := []dampenerSubject{}
	for scanner.Scan() {
		txt := scanner.Text()
		nums := strings.Split(txt, " ")
		ns := []int{}
		for _, s := range nums {
			n, _ := strconv.Atoi(s)
			ns = append(ns, n)
		}
		safe, pos := checkSafety(ns)
		if safe {
			countLevelSafe++
			continue
		}
		ds = append(ds, dampenerSubject{ns, pos})
	}

	dampenedCount := 0
	for _, d := range ds {
		ns1 := make([]int, len(d.ns))
		ns2 := make([]int, len(d.ns))
		ns3 := make([]int, len(d.ns))
		copy(ns1, d.ns)
		copy(ns2, d.ns)
		copy(ns3, d.ns)
		if d.pos == 0 {
			ns1 = ns1[1:]
			ns2 = []int{}
			ns3 = []int{}
		} else if d.pos > 0 {
			ns1 = slices.Delete(ns1, d.pos-1, d.pos)
			if d.pos+1 < len(d.ns) {
				ns2 = slices.Delete(ns2, d.pos, d.pos+1)
			} else if d.pos == len(d.ns)-1 {
				ns2 = ns2[:len(d.ns)-1]
			} else {
				ns2 = []int{}
			}
			if d.pos == 2 {
				ns3 = ns3[1:]
			} else {
				ns3 = []int{}
			}
		}
		if safe, _ := checkSafety(ns1); safe {
			dampenedCount++
		} else if safe, _ = checkSafety(ns2); safe {
			dampenedCount++
		} else if safe, _ = checkSafety(ns3); safe {
			dampenedCount++
		}
	}

	fmt.Println(countLevelSafe)
	fmt.Println(countLevelSafe + dampenedCount)
}

func checkSafety(ns []int) (bool, int) {
	safe := true
	if len(ns) == 0 {
		safe = false
		return safe, 0
	}
	prev := ns[0]
	curr := ns[1]
	trending := ""
	diff := curr - prev
	pos := 0
	if diff > 0 {
		trending = "increasing"
	} else if diff < 0 {
		trending = "decreasing"
	} else {
		safe = false
		return safe, 1
	}

	if math.Abs(float64(diff)) > 3 {
		safe = false
		return safe, 1
	}

	for i := 2; i < len(ns); i++ {
		prev = curr
		curr = ns[i]
		diff = curr - prev
		if diff == 0 ||
			(trending == "increasing" && diff < 0) ||
			(trending == "decreasing" && diff > 0) ||
			math.Abs(float64(diff)) > 3 {
			safe = false
			return safe, i
		}
	}
	return safe, pos
}
