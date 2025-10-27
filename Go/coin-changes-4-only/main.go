package main

import (
	"fmt"
	"slices"

	"github.com/mashingan/gastar"
)

type (
	coinChanges struct {
		coins []coin
		value int
	}
	coin    int
	changes struct {
		gastar.Grapher[string, coinChanges, coin]
	}
)

const (
	maxChanges   = 4
	targetChange = 37
)

var coins = []int{10, 9, 1}

func (c coinChanges) Hash() string {
	cc := slices.SortedFunc(slices.Values(c.coins), func(a, b coin) int {
		if int(a) >= int(b) {
			return -1
		}
		return 1
	})
	return fmt.Sprintf("%q:%d", cc, c.value)
}

func (ch changes) Neighbors(cc coinChanges) []coinChanges {
	if cc.value == targetChange {
		return []coinChanges{
			{value: targetChange},
		}
	}
	if len(cc.coins) >= maxChanges || cc.value > targetChange {
		return []coinChanges{}
	}

	currsum := 0
	for _, c := range cc.coins {
		currsum += int(c)
	}

	if currsum > targetChange {
		return []coinChanges{}
	}

	result := make([]coinChanges, 0, len(coins))
	for _, c := range coins {
		result = append(result, coinChanges{
			coins: append(cc.coins, coin(c)),
			value: cc.value + c,
		})
	}
	return result

}

func main() {
	var cg changes
	cg.Grapher = gastar.NewDefault[string, coinChanges, coin]()
	empty := coinChanges{value: 0}
	goal := coinChanges{value: targetChange}
	paths := gastar.PathFind[string, coinChanges, int](cg, empty, goal)
	if len(paths) <= 2 /* only empty and goal, nothing in between */ {
		fmt.Println("could not find anything between yet goal reached, check graph states")
		return
	}
	lastOptimumStateBeforeGoal := paths[len(paths)-2]
	fmt.Println("paths:", paths)
	fmt.Println("optimum coin changes:", lastOptimumStateBeforeGoal.coins)
}
