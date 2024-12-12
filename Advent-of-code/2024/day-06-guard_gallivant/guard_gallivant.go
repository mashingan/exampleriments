package main

import (
	"aoc-2024/common"
	"fmt"
	"log"
)

const (
	// fname = "sample.txt"
	// fname = "sample2.txt"
	fname = "input.txt"
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
			recordWalk(walked, g.pos, g.direction)
			return true
		}
		recordWalk(walked, g.pos, g.direction)
	}
	return false
}

const (
	up       direction = '^'
	right    direction = '>'
	down     direction = 'v'
	left     direction = '<'
	obstacle byte      = '#'
)

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
	ginit := guardinitial
	for guardinitial.walk(guardwalk, labmap) {
	}
	fmt.Println(len(guardwalk))
	nom := overlapMap(ginit, guardwalk, labmap)
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

func outsideMap(labmap [][]byte, g1 guard) bool {
	return g1.row < 0 || g1.row >= len(labmap) || g1.col < 0 || g1.col >= len(labmap[g1.row])
}

func overlapMap(ginit guard, wm map[pos][]direction, labmap [][]byte) map[pos]struct{} {
	newObstacleMap := map[pos]struct{}{}
	for p := range wm {
		if p == ginit.pos {
			continue
		}
		labmap[p.row][p.col] = obstacle
		count := 0
		wall := []pos{}
		walked := map[pos][]direction{}
		g1 := ginit
		for count < 3 {
			if !g1.walk(walked, labmap) {
				break
			}
			if in(wall, g1.pos) {
				count++
			}
			wall = append(wall, g1.pos)
		}
		labmap[p.row][p.col] = '.'
		if count >= 3 {
			newObstacleMap[p] = struct{}{}
		}
	}
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
