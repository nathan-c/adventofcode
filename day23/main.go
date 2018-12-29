package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"regexp"
	"sort"
	"strconv"
	"strings"
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

func (a nanoBot) isInRange(b location) bool {
	dist := abs(a.x-b.x) + abs(a.y-b.y) + abs(a.z-b.z)
	return dist <= a.r
}

func (a nanoBot) countPossiblePointsInRange() int {
	return a.r * a.r * a.r
}

func (a nanoBot) inRangeLocations() (count int) {
	minx := a.x - a.r
	maxx := a.x + a.r
	miny := a.y - a.r
	maxy := a.y + a.r
	minz := a.z - a.r
	maxz := a.z + a.r

	for x := minx; x < maxx; x++ {
		for y := miny; y < maxy; y++ {
			for z := minz; z < maxz; z++ {
				l := location{x, y, z}
				if a.isInRange(l) {
					count++
				}
			}
		}
	}
	return
}

type botMap map[nanoBot]map[nanoBot]struct{}

func (m botMap) add(a, b nanoBot) {
	var set map[nanoBot]struct{}
	if s, ok := m[a]; ok {
		set = s
	} else {
		set = make(map[nanoBot]struct{})
		m[a] = set
	}

	set[b] = struct{}{}

	if s, ok := m[b]; ok {
		set = s
	} else {
		set = make(map[nanoBot]struct{})
		m[b] = set
	}
	set[a] = struct{}{}
}

func (m botMap) String() string {
	list := make([]struct {
		bot   nanoBot
		count int
	}, len(m))

	for k, v := range m {
		list = append(list, struct {
			bot   nanoBot
			count int
		}{k, len(v)})
	}

	sort.Slice(list, func(i, j int) bool {
		return list[i].count < list[j].count
	})

	var sb strings.Builder
	for _, x := range list {
		sb.WriteString(fmt.Sprintf("%v: count %v\n", x.bot, x.count))
	}
	return sb.String()
}

func (m botMap) getMaxOverlappingBots() (int, []nanoBot) {
	maxOverlap := 0
	var botsWithMaxOverlap []nanoBot

	for k, v := range m {
		if len(v) == maxOverlap {
			botsWithMaxOverlap = append(botsWithMaxOverlap, k)
		} else if len(v) > maxOverlap {
			maxOverlap = len(v)
			botsWithMaxOverlap = []nanoBot{k}
		}
	}
	return maxOverlap, botsWithMaxOverlap
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
		if maxRad.isInRange(x.location) {
			count++
		}
	}
	return count
}

func part2(bots []nanoBot) int {
	minx, maxx, miny, maxy, minz, maxz := math.MaxInt64, math.MinInt64, math.MaxInt64, math.MinInt64, math.MaxInt64, math.MinInt64
	for _, bot := range bots {
		minx = min(minx, bot.x)
		miny = min(miny, bot.y)
		minz = min(minz, bot.z)
		maxx = max(maxx, bot.x)
		maxy = max(maxy, bot.y)
		maxz = max(maxz, bot.z)
	}
	fmt.Printf("x=(%v..%v) y=(%v..%v) z=(%v..%v)\n", minx, maxx, miny, maxy, minz, maxz)
	fmt.Printf("volume: %v\n", (maxx-minx)*(maxy-miny)*(maxz-minz))
	// because search space is too large i can reduce it by only looking at areas where 2 satellites have overlapping range
	// :( still too big
	overlappingBots := make(botMap)
	for _, x := range bots {
		for _, y := range bots {
			if x == y {
				continue
			}
			if rangesOverlap(x, y) {
				overlappingBots.add(x, y)
			}
		}
	}
	fmt.Printf("overlapping bots: %v\n", overlappingBots)
	overlapSize, maxbots := overlappingBots.getMaxOverlappingBots()
	fmt.Printf("max overlapping bots %v:\n%v\n", overlapSize, maxbots)
	//searchSpace := make(map[location]struct{})
	sort.Slice(bots, func(i, j int) bool {
		return bots[i].r < bots[j].r
	})
	for _, b := range bots {
		fmt.Println(b)
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
			location: location{
				x: atoi(botVals[1]),
				y: atoi(botVals[2]),
				z: atoi(botVals[3]),
			},
			r: atoi(botVals[4]),
		}
		bots = append(bots, newBot)
	}
	return bots
}
