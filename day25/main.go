package main

import (
	"io/ioutil"
	"regexp"
	"strings"
)

type stepInternal struct {
	WriteValue      int8
	MoveDirection   int
	NextInstruction rune
}

type step struct {
	ID        rune
	Internals [2]stepInternal
}

func main() {
	dat, _ := ioutil.ReadFile("day25.input.txt")
	stringFile := string(dat)
	blueprints := parseInput(stringFile)
	println(runPart1(blueprints, 'A', 12964419))
}

func runPart1(blueprint map[rune]step, beginState rune, steps int) int {
	tape := make(map[int]int8)
	position := 0
	tape[position] = 0
	currentState := blueprint[beginState]
	for i := 0; i < steps; i++ {
		step := currentState.Internals[tape[position]]
		tape[position] = step.WriteValue
		position += step.MoveDirection
		currentState = blueprint[step.NextInstruction]
	}
	checksum := 0
	for _, v := range tape {
		if v == 1 {
			checksum += int(v)
		}
	}
	return checksum
}

func parseInput(blueprintFile string) map[rune]step {
	rx := regexp.MustCompile(`(?m:In state (?P<state>\w):\s+If the current value is (?P<currentValue0>\d+).\s+- Write the value (?P<writeValue0>\d+).\s+- Move one slot to the (?P<direction0>right|left).\s+- Continue with state (?P<nextInstruction0>\w).\s+If the current value is (?P<currentValue1>\d+).\s+- Write the value (?P<writeValue1>\d+).\s+- Move one slot to the (?P<direction1>right|left).\s+- Continue with state (?P<nextInstruction1>\w))`)
	blueprints := make(map[rune]step)
	blocks := strings.Split(blueprintFile, "\n\n")
	for _, block := range blocks[1:] {
		matches := getMatches(rx, block)
		step := step{
			rune(matches["state"][0]),
			[2]stepInternal{
				{zeroOrOne(matches["writeValue0"]), leftOrRight(matches["direction0"]), rune(matches["nextInstruction0"][0])},
				{zeroOrOne(matches["writeValue1"]), leftOrRight(matches["direction1"]), rune(matches["nextInstruction1"][0])},
			},
		}
		blueprints[step.ID] = step
	}
	return blueprints
}

func leftOrRight(input string) int {
	if input == "left" {
		return -1
	}
	return 1
}

func zeroOrOne(input string) int8 {
	if input == "0" {
		return 0
	}
	return 1
}

func getMatches(myExp *regexp.Regexp, text string) map[string]string {
	match := myExp.FindStringSubmatch(text)
	result := make(map[string]string)
	for i, name := range myExp.SubexpNames() {
		if i != 0 {
			val := match[i]
			result[name] = val
		}
	}
	return result
}
