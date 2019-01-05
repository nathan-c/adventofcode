package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type point struct {
	x, y, z, t int
}

func dist(a, b *point) int {
	return abs(a.x-b.x) + abs(a.y-b.y) + abs(a.z-b.z) + abs(a.t-b.t)
}

func abs(a int) int {
	if a < 0 {
		return -a
	}
	return a
}

func main() {
	points := parseInput("input.txt")
	fmt.Printf("part one: %v constellations", part1(points))
}

func part1(points []*point) int {
	uf := newUnionFind(len(points))
	toI := make(map[*point]int)
	for i, p1 := range points {
		toI[p1] = i
		for p2, j := range toI {
			if dist(p1, p2) <= 3 {
				uf.merge(i, j)
			}
		}
	}
	return uf.numSets
}

func parseInput(fileName string) []*point {

	f, err := os.Open(fileName)
	if err != nil {
		log.Panicf("could not open file. %v", err)
	}
	defer f.Close()
	atoi := func(x string) int {
		i, _ := strconv.Atoi(x)
		return i
	}
	scanner := bufio.NewScanner(f)
	var points []*point
	for scanner.Scan() {
		splitLine := strings.Split(scanner.Text(), ",")
		pt := &point{
			x: atoi(splitLine[0]),
			y: atoi(splitLine[1]),
			z: atoi(splitLine[2]),
			t: atoi(splitLine[3]),
		}
		points = append(points, pt)
	}
	return points
}
