package main

import (
	"aoc-2024/common"
	"fmt"
	"log"
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
	incNext := func(d direction) {
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
	goBack := func(d direction) {
		for range 2 {
			d = nextDirection[d]
		}
		incNext(d)
	}
	for ; limitTest(); incNext(g.direction) {
		if labmap[g.row][g.col] == obstacle {
			goBack(g.direction)
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

var notAvailablePos = pos{-1, -1}

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	// const fname = "sample.txt"
	const fname = "input.txt"
	row := 0
	labmap := [][]byte{}
	var guardinitial guard
	guardwalk := map[pos][]direction{}
	obstacleMap := map[pos]struct{}{}
	common.ReadLines(fname, func(text string) {
		defer func() { row++ }()
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
	fmt.Println(len(guardwalk))
}

func recordWalk(walked map[pos][]direction, p pos, d direction) {
	prev, ok := walked[p]
	if ok {
		prev = append(prev, up)
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
			}
		}
		if g.direction == right {
		walkRight:
			for x+1 < len(labmap[y]) {
				p := labmap[y][x+1]
				upos := (pos{x, y})
				// log.Printf("p: %c, direction: %c, pos: %#v\n", p, g.direction, upos)
				if p == obstacle {
					g.direction = 'v'
					g.pos = upos
					break walkRight
				}
				walked[upos] = struct{}{}
				x++
			}
			if x == len(labmap[y])-1 {
				walked[pos{x, y}] = struct{}{}
				x++
			}
		}
		if g.direction == down {
		walkDown:
			for y+1 < len(labmap) {
				p := labmap[y+1][x]
				upos := (pos{x, y})
				// log.Printf("p: %c, direction: %c, pos: %#v\n", p, g.direction, upos)
				if p == obstacle {
					g.direction = '<'
					g.pos = upos
					break walkDown
				}
				walked[upos] = struct{}{}
				y++
			}
			if y == len(labmap)-1 {
				walked[pos{x, y}] = struct{}{}
				y++
			}
		}
		if g.direction == left {
		walkLeft:
			for x-1 >= 0 {
				p := labmap[y][x-1]
				upos := (pos{x, y})
				// log.Printf("p: %c, direction: %c, pos: %#v\n", p, g.direction, upos)
				if p == obstacle {
					g.direction = '^'
					g.pos = upos
					break walkLeft
				}
				walked[upos] = struct{}{}
				x--
			}
			if x == 0 {
				walked[pos{x, y}] = struct{}{}
				x--
			}
		}
	}
}
