package main

import (
	"aoc-2024/common"
	"fmt"
	"log"
)

const (
	fname = "sample.txt"
	// fname = "sample2.txt"
	// fname = "input.txt"
)

type (
	pos struct {
		col, row int
	}

	direction byte
	guard     struct {
		direction
		pos
	}
)

var nextDirection = map[direction]direction{
	up:    right,
	right: down,
	down:  left,
	left:  up,
}

func (g *guard) step(d direction) {
	switch d {
	case up:
		g.row--
	case right:
		g.col++
	case down:
		g.row++
	default:
		g.col--
	}
}

func (g *guard) goBack(d direction) {
	for range 2 {
		d = nextDirection[d]
	}
	g.step(d)
}

func (g *guard) walk(walked map[pos][]direction, labmap [][]byte) bool {
	limitTest := func() bool {
		switch g.direction {
		case up:
			return g.row >= 0
		case right:
			return g.col < len(labmap[g.row])
		case down:
			return g.row < len(labmap)
		default:
			return g.row >= 0
		}
	}
	for ; limitTest(); g.step(g.direction) {
		if outsideMap(labmap, *g) {
			return false
		}
		if labmap[g.row][g.col] == obstacle {
			g.goBack(g.direction)
			g.direction = nextDirection[g.direction]
			log.Printf("before recordwalk: pos: %#v, direction: %q\n", g.pos, walked[g.pos])
			recordWalk(walked, g.pos, g.direction)
			log.Printf("after  recordwalk: pos: %#v, direction: %q\n", g.pos, walked[g.pos])
			return true
		}
		recordWalk(walked, g.pos, g.direction)
	}
	return false
}

func (g guard) before(p pos) bool {
	switch g.direction {
	case up:
		return g.row > p.row
	case right:
		return g.col < p.col
	case down:
		return g.row < p.row
	default:
		return g.col > p.col
	}
}

const (
	up       direction = '^'
	right    direction = '>'
	down     direction = 'v'
	left     direction = '<'
	obstacle byte      = '#'
)

var notAvailablePos = pos{-1, -1}

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	row := 0
	labmap := [][]byte{}
	var guardinitial guard
	guardwalk := map[pos][]direction{}
	obstacleMap := map[pos]struct{}{}
	common.ReadLines(fname, func(text string) {
		defer func() { row++ }()
		if text == "" {
			return
		}
		bs := []byte(text)
		labmap = append(labmap, bs)
		for c, b := range bs {
			p := (pos{c, row})
			switch direction(b) {
			case up:
				fallthrough
			case right:
				fallthrough
			case down:
				fallthrough
			case left:
				{
					guardinitial = guard{direction: direction(b), pos: p}
					guardwalk[p] = []direction{direction(b)}
				}
			}
			if b == obstacle {
				obstacleMap[p] = struct{}{}
			}
		}
	})
	walk(guardinitial, labmap, guardwalk)
	// log.Println(err)
	// log.Println(guardinitial)
	// log.Println(guardwalk)
	// for _, r := range labmap {
	// 	log.Println(string(r))
	// }
	// fmt.Println(len(guardwalk))
	// for o := range obstacleMap {
	// 	log.Println("obstacle:", o)
	// }
	nom := overlapMap(guardinitial, obstacleMap, guardwalk, labmap)
	for p := range nom {
		log.Println("nom:", p)
	}
	fmt.Println(len(nom))
}

func recordWalk(walked map[pos][]direction, p pos, d direction) {
	prev, ok := walked[p]
	if ok {
		prev = append(prev, d)
		walked[p] = prev
	} else {
		walked[p] = []direction{d}
	}
}

func walk(g guard, labmap [][]byte, walked map[pos][]direction) {
	running := true
	for running {
		running = g.walk(walked, labmap)
	}
}

func checkPos(g1 guard) {
	log.Println("g1.pos:", g1.pos, ", direction:", string(g1.direction))
}
func outsideMap(labmap [][]byte, g1 guard) bool {
	return g1.row < 0 || g1.row >= len(labmap) || g1.col < 0 || g1.col >= len(labmap[g1.row])
}

func adjacentOf(p, a pos) bool {
	return (p.col == a.col+1 && p.row == a.row) ||
		(p.col == a.col-1 && p.row == a.row) ||
		(p.col == a.col && p.row == a.row+1) ||
		(p.col == a.col && p.row == a.row-1)

}

func overlapMap(ginit guard, om map[pos]struct{}, wm map[pos][]direction, labmap [][]byte) map[pos]struct{} {
	newObstacleMap := map[pos]struct{}{}
	dumwalk := map[pos][]direction{}
	for y, row := range labmap {
		for x, col := range labmap[y] {
		}
	}
	// for k := range om {
	// 	log.Println("obstacle:", k)
	// 	mpaths := map[direction]pos{
	// 		down:  {k.col - 1, k.row}, // next down direction, from left to right
	// 		right: {k.col, k.row + 1}, // next right direction, from down to up
	// 		up:    {k.col + 1, k.row}, // next up direction, from right to left
	// 		left:  {k.col, k.row - 1}, // next left direction, from up to down
	// 	}
	// 	walked := map[pos][]direction{}
	// probeAdjacents:
	// 	for d, p := range mpaths {
	// 		// log.Println("d before:", string(d))
	// 		// d = nextDirection[d]
	// 		// log.Println("d  after:", string(d))
	// 		log.Println("adjacent obstacle:", p)
	// 		w, ok := wm[p]
	// 		if !ok || !in(w, d) {
	// 			log.Printf("skipped: dir %c with walked %q\n", d, w)
	// 			continue
	// 		}
	// 		g1 := guard{d, p}
	// 		if outsideMap(labmap, g1) {
	// 			continue
	// 		}
	// 		for range 2 {
	// 			if !g1.walk(walked, labmap) {
	// 				continue probeAdjacents
	// 			}
	// 			checkPos(g1)
	// 		}
	// 		g1d := g1.direction
	// 		for {
	// 			if outsideMap(labmap, g1) {
	// 				continue probeAdjacents
	// 			}
	// 			if labmap[g1.row][g1.col] == obstacle {
	// 				continue probeAdjacents
	// 			}
	// 			g1.step(g1d)
	// 			checkPos(g1)
	// 			if g1.col == p.col || g1.row == p.row {
	// 				break
	// 			}
	// 		}
	// 		g1.step(g1d)
	// 		checkPos(g1)
	// 		if outsideMap(labmap, g1) {
	// 			continue
	// 		}
	// 		if labmap[g1.row][g1.col] != obstacle || g1.pos != ginit.pos {
	// 			log.Println("found possible new obstruction:", g1.pos)
	// 			newObstacleMap[g1.pos] = struct{}{}
	// 		}
	// 	}
	// }
	return newObstacleMap
}

func in[T comparable](arr []T, e T) bool {
	for _, a := range arr {
		if a == e {
			return true
		}
	}
	return false
}
