package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"regexp"
	"strconv"
)

var rx = `pos=<([-\d]+),([-\d]+),([-\d]+)>, r=([-\d]+)`

func main() {
	bots := readInput("input.txt")
	botsInRange := part1(bots)
	fmt.Printf("part one: %v bots in range\n", botsInRange)
	part2(bots)
}

type location struct {
	x, y, z int
}

type nanoBot struct {
	location
	r int
}

func (a nanoBot) isInRange(b nanoBot) bool {
	dist := abs(a.x-b.x) + abs(a.y-b.y) + abs(a.z-b.z)
	return dist <= a.r
}

func rangesOverlap(a, b nanoBot) bool {
	dist := abs(a.x-b.x) + abs(a.y-b.y) + abs(a.z-b.z)
	return dist <= a.r+b.r
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func min(a, b int) int {
	if a > b {
		return b
	}
	return a
}

func max(a, b int) int {
	if a < b {
		return b
	}
	return a
}

func part1(bots []nanoBot) int {
	var maxRad nanoBot
	for _, x := range bots {
		if x.r > maxRad.r {
			maxRad = x
		}
	}
	count := 0
	for _, x := range bots {
		if maxRad.isInRange(x) {
			count++
		}
	}
	return count
}

func part2(bots []nanoBot) int {
	minx, maxx, miny, maxy, minz, maxz := math.MaxInt32, math.MinInt32, math.MaxInt32, math.MinInt32, math.MaxInt32, math.MinInt32
	for _, bot := range bots {
		minx = min(minx, bot.x)
		miny = min(miny, bot.y)
		minz = min(minz, bot.z)
		maxx = max(maxx, bot.x)
		maxy = max(maxy, bot.y)
		maxz = max(maxz, bot.z)
	}
	fmt.Printf("x=(%v..%v) y=(%v..%v) z=(%v..%v)\n", minx, maxx, miny, maxy, minz, maxz)
	fmt.Printf("volume: %v", (maxx-minx)*(maxy-miny)*(maxz-minz))
	// because search space is too large i can reduce it by only looking at areas where 2 satellites have overlapping range
	searchSpace := make(map[location]bool)
	for _, x := range bots {
		for _, y := range bots {
			if x == y {
				continue
			}
			if rangesOverlap(x, y) {

			}
		}
	}
	return 0
}

func readInput(fileName string) []nanoBot {
	f, err := os.Open(fileName)
	if err != nil {
		log.Panicf("could not open file. %v", err)
	}
	defer f.Close()
	atoi := func(x string) int {
		i, _ := strconv.Atoi(x)
		return i
	}
	rx, _ := regexp.Compile(rx)
	scanner := bufio.NewScanner(f)
	var bots []nanoBot
	for scanner.Scan() {
		botVals := rx.FindStringSubmatch(scanner.Text())
		newBot := nanoBot{
			x: atoi(botVals[1]),
			y: atoi(botVals[2]),
			z: atoi(botVals[3]),
			r: atoi(botVals[4]),
		}
		bots = append(bots, newBot)
	}
	return bots
}
