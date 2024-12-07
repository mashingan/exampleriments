//go:build goexperiment.arenas

package main

// to build:
// go build -tags goexperiment.arenas btree-arena.go
// time ./btree-arena 21

import (
	"arena"
	"flag"
	"fmt"
	"strconv"
	"sync"
)

type Tree struct {
	Left, Right *Tree
}

func (t *Tree) Count() int {
	if t.Left == nil {
		return 1
	}
	return 1 + t.Right.Count() + t.Left.Count()
}

func NewTree(a *arena.Arena, depth int) *Tree {
	if depth > 0 {
		t := arena.New[Tree](a)
		depth--
		t.Left = NewTree(a, depth)
		t.Right = NewTree(a, depth)
		return t
	}
	return arena.New[Tree](a)
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
		a := arena.NewArena()
		tree := NewTree(a, maxdepth+1)
		msg := fmt.Sprintf("Stretch tree of depth: %d\t check: %d",
			maxdepth+1, tree.Count())
		outBuff[0] = msg
		a.Free()
	}(&wg)

	var longLivedTree *Tree
	la := arena.NewArena()
	wg.Add(1)
	go func(w *sync.WaitGroup) {
		defer w.Done()
		longLivedTree = NewTree(la, maxdepth)
	}(&wg)

	for depth := mindepth; depth <= maxdepth; depth += 2 {
		iter := 1 << (maxdepth - depth + mindepth)
		outCurr++

		wg.Add(1)
		go func(w *sync.WaitGroup, depth, iter, index int) {
			defer w.Done()
			acc := 0
			for i := 0; i < iter; i++ {
				ar := arena.NewArena()
				a := NewTree(ar, depth)
				acc += a.Count()
				ar.Free()
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
	la.Free()

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
