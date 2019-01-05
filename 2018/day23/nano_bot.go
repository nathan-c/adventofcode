package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

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

func (a nanoBot) String() string {
	return fmt.Sprintf("pos=<%v,%v,%v>, r=%v", a.x, a.y, a.z, a.r)
}

func fromString(input string) nanoBot {
	var x int
	var y int
	var z int
	var r int

	fmt.Sscanf(input, "pos=<%d,%d,%d>, r=%d", &x, &y, &z, &r)
	return nanoBot{location{x, y, z}, r}
}

func readInput(fileName string) []nanoBot {
	f, err := os.Open(fileName)
	if err != nil {
		log.Panicf("could not open file. %v", err)
	}
	defer f.Close()
	scanner := bufio.NewScanner(f)
	var bots []nanoBot
	for scanner.Scan() {
		newBot := fromString(scanner.Text())
		bots = append(bots, newBot)
	}
	return bots
}
