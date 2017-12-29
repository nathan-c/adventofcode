package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func main() {
	bytes, _ := ioutil.ReadFile("day16.input.txt")
	input := string(bytes)
	programs := getPrograms(16)
	for instruction := range parseInput(input) {
		instruction(programs)
	}
	fmt.Println(programs)

	// Part 2
	fmt.Println(part2(input, 16, 1E9))
}

type holder struct {
	string
	firstSeen int
}

func part2(input string, size int, iterations int) string {
	var instructions []func(*programs)
	for instruction := range parseInput(input) {
		instructions = append(instructions, instruction)
	}
	programs := getPrograms(size)
	memo := make(map[string]holder)
	cycleSize := 0
	for i := 0; i < iterations; i++ {
		if val, ok := memo[programs.String()]; ok {
			cycleSize = len(memo) - val.firstSeen
			remainder := (iterations - 1) % cycleSize
			for _, out := range memo {
				if out.firstSeen == remainder {
					return out.string
				}
			}
		}
		before := programs.String()
		for _, instruction := range instructions {
			instruction(programs)
		}
		fmt.Printf("%v: %v -> %v\n", i, before, programs.String())
		memo[before] = holder{programs.String(), i}
	}
	return programs.String()
}

func parseInput(input string) <-chan func(*programs) {
	ch := make(chan func(*programs))
	go func() {
		instructions := strings.Split(input, ",")
		for _, instruction := range instructions {
			i := rune(instruction[0])
			switch i {
			case 's':
				number := parseSpin(instruction)
				ch <- func(x *programs) { spin(x, number) }
				break
			case 'x':
				a, b := parseExchange(instruction)
				ch <- func(x *programs) { exchange(x, a, b) }
				break
			case 'p':
				a, b := parsePartner(instruction)
				ch <- func(x *programs) { partner(x, a, b) }
				break
			}
		}
		close(ch)
	}()

	return ch
}

func parseSpin(instruction string) int {
	number, _ := strconv.Atoi(instruction[1:])
	return number
}

func parseExchange(instruction string) (int, int) {
	parts := strings.Split(instruction[1:], "/")
	a, _ := strconv.Atoi(parts[0])
	b, _ := strconv.Atoi(parts[1])
	return a, b
}

func parsePartner(instruction string) (rune, rune) {
	return rune(instruction[1]), rune(instruction[3])
}

func spin(input *programs, number int) {
	input.moveStart(input.len() - number)
}

func exchange(input *programs, a int, b int) {
	pA := input.get(a)
	input.set(a, input.get(b))
	input.set(b, pA)
}

func partner(input *programs, a rune, b rune) {
	indices := input.find(a, b)
	exchange(input, indices[0], indices[1])
}

func charToNum(r rune) int {
	if '0' <= r && r <= '9' {
		return int(r) - '0'
	}
	return 0
}

func charByteToNum(b byte) int {
	r := rune(b)
	if '0' <= r && r <= '9' {
		return int(r) - '0'
	}
	return 0
}
