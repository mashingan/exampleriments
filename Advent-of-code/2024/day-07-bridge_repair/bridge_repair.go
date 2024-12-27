package main

import (
	"aoc-2024/common"
	"fmt"
	"log"
	"math/big"
	"strconv"
	"strings"

	"github.com/mashingan/gastar"
)

const (
	input = "input.txt"
	// input = "sample.txt"
)

type node struct {
	val     uint64
	op      byte
	deep    uint
	current *big.Int
	total   *big.Int
}

func (n node) String() string {
	return fmt.Sprintf("{val:%d,op:%c,deep:%d,current:%d,total:%d}",
		n.val, n.op, n.deep, n.current.Uint64(), n.total.Uint64(),
	)
}

func (m node) Hash() string {
	return fmt.Sprintf("%d|%d", m.current.Uint64(), m.total.Uint64())
}

type gn struct {
	gastar.Grapher[node, node, int]
	paths []*big.Int
}

func (g gn) Neighbors(n node) []node {
	if n.deep >= uint(len(g.paths)) {
		return []node{}
	}
	currentp := big.NewInt(n.current.Int64())
	currentm := big.NewInt(n.current.Int64())
	val := n.val
	if n.deep == 0 {
		currentp = big.NewInt(g.paths[0].Int64())
		currentm = big.NewInt(g.paths[0].Int64())
	}
	if n.deep+1 < uint(len(g.paths)) {
		val = g.paths[n.deep+1].Uint64()
		currentp = currentp.Add(currentp, g.paths[n.deep+1])
		currentm = currentm.Mul(currentm, g.paths[n.deep+1])
	}
	return []node{
		{val: val, deep: n.deep + 1, op: '+', current: currentp, total: n.total},
		{val: val, deep: n.deep + 1, op: '*', current: currentm, total: n.total},
	}
}

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	tots := big.NewInt(0)
	count := 0
	validcount := 0
	common.ReadLines(input, func(text string) {
		nums := strings.Split(text, " ")
		vals := make([]*big.Int, len(nums)-1)
		sum1, _ := strconv.Atoi(strings.Trim(nums[0], ":"))
		sum := big.NewInt(int64(sum1))
		for i := 1; i < len(nums); i++ {
			n, _ := strconv.Atoi(strings.TrimSpace(nums[i]))
			vals[i-1] = big.NewInt(int64(n))
		}

		count++
		start := node{
			total:   sum,
			current: big.NewInt(0),
		}
		goal := node{
			total:   sum,
			current: sum,
		}
		g := gn{Grapher: gastar.NewDefault[node, node, int]()}
		g.paths = vals
		paths := gastar.PathFind[node, node](g, start, goal)
		if len(paths) <= 0 {
			return
		}
		validcount++
		tots.Add(tots, sum)
		// log.Printf("count: %d, %q\n", count, paths)
		fmt.Printf("line: %d, the sum: %d, current sum: %d\n", count, sum.Uint64(), tots.Uint64())
	})
	fmt.Println(tots.Uint64())
	fmt.Println("validcount:", validcount)
}
