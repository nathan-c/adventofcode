package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	instructions := readInput()
	grid := make(map[[2]int]bool)
	for _, instruction := range instructions {
		grid[instruction] = !grid[instruction]
	}

	blackCount := 0
	for _, v := range grid {
		if v {
			blackCount++
		}
	}
	fmt.Printf("Part 1: %v\n", blackCount)
	for i := 0; i < 100; i++ {
		grid = runOne(grid)
	}
	blackCount = 0
	for _, v := range grid {
		if v {
			blackCount++
		}
	}
	fmt.Printf("Part 2: %v\n", blackCount)
}

func runOne(grid map[[2]int]bool) map[[2]int]bool {
	minx, miny, maxx, maxy := getBounds(grid)
	newGrid := make(map[[2]int]bool)
	for x := minx; x < maxx+1; x++ {
		for y := miny; y < maxy+1; y++ {
			coord := [2]int{x, y}
			black, _ := grid[coord]
			blackNeighbours := countBlackNeighbours(coord, grid)
			if black && (blackNeighbours == 1 || blackNeighbours == 2) {
				newGrid[coord] = true
			}
			if !black && blackNeighbours == 2 {
				newGrid[coord] = true
			}
		}
	}
	return newGrid
}

func countBlackNeighbours(coord [2]int, grid map[[2]int]bool) int {
	count := 0
	for _, t := range directionToCoord {
		c := [2]int{coord[0] + t[0], coord[1] + t[1]}
		if black, ok := grid[c]; ok && black {
			count++
		}
	}
	return count
}

func getBounds(grid map[[2]int]bool) (minx, miny, maxx, maxy int) {
	for coord, flipped := range grid {
		if !flipped {
			continue
		}
		if minx > coord[0] {
			minx = coord[0]
		}
		if miny > coord[1] {
			miny = coord[1]
		}
		if maxx < coord[0] {
			maxx = coord[0]
		}
		if maxy < coord[1] {
			maxy = coord[1]
		}
	}
	minx--
	miny--
	maxx++
	maxy++
	return
}

func readInput() [][2]int {

	f, _ := os.Open("input")
	defer f.Close()

	var instructions [][2]int

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		instructions = append(instructions, lineToCoord(line))
	}

	return instructions
}

func lineToCoord(line string) [2]int {

	var final [2]int
	for len(line) > 0 {
		var coord [2]int
		if line[0] == 'n' || line[0] == 's' {
			coord = directionToCoord[line[:2]]
			line = line[2:]
		} else {
			coord = directionToCoord[line[:1]]
			line = line[1:]
		}
		final[0], final[1] = final[0]+coord[0], final[1]+coord[1]

	}
	return final
}

var directionToCoord map[string][2]int = map[string][2]int{
	"e":  {1, 0},
	"w":  {-1, 0},
	"ne": {1, 1},
	"nw": {0, 1},
	"se": {0, -1},
	"sw": {-1, -1},
}
