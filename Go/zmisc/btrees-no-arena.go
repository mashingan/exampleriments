package main

import (
	"flag"
	"fmt"
	"strconv"
	"sync"
)

// to build and run with time
// go build btree-no-arena.go
// time ./btree-no-arena 21

type Tree struct {
	Left, Right *Tree
}

func (t *Tree) Count() int {
	if t.Left == nil {
		return 1
	}
	return 1 + t.Right.Count() + t.Left.Count()
}

func NewTree(depth int) *Tree {
	if depth > 0 {
		return &Tree{Left: NewTree(depth - 1), Right: NewTree(depth - 1)}
	}
	return &Tree{}
}

func Run(maxdepth int) {
	var wg sync.WaitGroup
	const mindepth = 4
	maxdepth = max(maxdepth, mindepth+2)

	outCurr := 0
	outSize := 3 + (maxdepth-mindepth)/2
	outBuff := make([]string, outSize)

	wg.Add(1)
	go func(w *sync.WaitGroup) {
		defer w.Done()
		tree := NewTree(maxdepth + 1)
		msg := fmt.Sprintf("Stretch tree of depth: %d\t check: %d",
			maxdepth+1, tree.Count())
		outBuff[0] = msg
	}(&wg)

	var longLivedTree *Tree
	wg.Add(1)
	go func(w *sync.WaitGroup) {
		defer w.Done()
		longLivedTree = NewTree(maxdepth)
	}(&wg)

	for depth := mindepth; depth <= maxdepth; depth += 2 {
		iter := 1 << (maxdepth - depth + mindepth)
		outCurr++

		wg.Add(1)
		go func(w *sync.WaitGroup, depth, iter, index int) {
			defer w.Done()
			acc := 0
			for i := 0; i < iter; i++ {
				a := NewTree(depth)
				acc += a.Count()
			}
			msg := fmt.Sprintf("%d\t trees of depth %d\t check %d",
				iter, depth, acc)
			outBuff[index] = msg
		}(&wg, depth, iter, outCurr)
	}
	wg.Wait()

	msg := fmt.Sprintf("long lived tree of depth %d\t check: %d",
		maxdepth, longLivedTree.Count())
	outBuff[outSize-1] = msg

	for _, m := range outBuff {
		fmt.Println(m)
	}
}

func main() {
	n := 0
	flag.Parse()
	if flag.NArg() > 0 {
		n, _ = strconv.Atoi(flag.Arg(0))
	}
	Run(n)
}
