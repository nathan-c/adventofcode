package main

import (
	"fmt"
)

var depth = 6969
var target = location{9, 796}

func main() {
	c := newCaves(depth, target)
	risk := part1(c)
	fmt.Printf("part 1: %v\n", risk)
	shortestPathLength := part2(c)
	fmt.Printf("part 2: %v\n", shortestPathLength)
}

func part1(c caves) int {
	sumz := 0
	for x := 0; x <= c.target.x; x++ {
		for y := 0; y <= c.target.y; y++ {
			er := c.erosionLevel(location{x, y})
			sumz += er % 3
		}
	}
	return sumz
}

func part2(c caves) int {
	_, cost, _ := shortestPath(c, locationAndEquipment{location{0, 0}, torch}, locationAndEquipment{c.target, torch})
	return cost
}
