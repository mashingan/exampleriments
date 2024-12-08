package main

import (
	"aoc-2024/common"
	"fmt"
	"log"
)

type pos struct {
	col, row int
}

type direction byte

const (
	up       direction = '^'
	right    direction = '>'
	down     direction = 'v'
	left     direction = '<'
	obstacle byte      = '#'
)

type guard struct {
	direction
	pos
}

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	// const fname = "sample.txt"
	const fname = "input.txt"
	row := 0
	labmap := [][]byte{}
	var guardinitial guard
	guardFound := false
	guardwalk := map[pos]struct{}{}
	common.ReadLines(fname, func(text string) {
		defer func() { row++ }()
		bs := []byte(text)
		labmap = append(labmap, bs)
		if !guardFound {
			for c, b := range bs {
				switch direction(b) {
				case up:
					fallthrough
				case right:
					fallthrough
				case down:
					fallthrough
				case left:
					{
						guardFound = true
						p := (pos{c, row})
						guardinitial = guard{direction: direction(b), pos: p}
						guardwalk[p] = struct{}{}
					}
				}
				if guardFound {
					break
				}
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

func walk(g guard, labmap [][]byte, walked map[pos]struct{}) {
	y := g.row
	x := g.col
	for y >= 0 && y < len(labmap) && x >= 0 && x < len(labmap[y]) {
		if g.direction == up {
		walkUp:
			for y-1 >= 0 {
				p := labmap[y-1][x]
				upos := (pos{x, y})
				// log.Printf("p: %c, direction: %c, pos: %#v\n", p, g.direction, upos)
				if p == obstacle {
					g.direction = '>'
					g.pos = upos
					break walkUp
				}
				walked[upos] = struct{}{}
				y--
			}
			if y == 0 {
				walked[pos{x, y}] = struct{}{}
				y--
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
