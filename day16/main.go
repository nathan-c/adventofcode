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
	programs := *getPrograms(16)
	for instruction := range parseInput(input) {
		instruction(&programs)
	}
	fmt.Println(string(programs))

	// Part 2
	programsStart := buildMap(*getPrograms(16))
	programsEnd := buildMap(programs)
	startToEnd := make(map[int]int)
	for prog, pos := range programsStart {
		startToEnd[pos] = programsEnd[prog]
	}

	programRef := getPrograms(16)
	temp := make([]rune, 16)
	memo := make(map[string]string)
	for i := 0; i < 1E8; i++ {
		mapValues(programRef, &temp, startToEnd, memo)
	}
	fmt.Println(string(*programRef))
}

func part2(input string, size int, iterations int) string {
	programs := *getPrograms(size)
	for instruction := range parseInput(input) {
		instruction(&programs)
	}
	// Part 2
	programsStart := buildMap(*getPrograms(size))
	programsEnd := buildMap(programs)
	startToEnd := make(map[int]int)
	for prog, pos := range programsStart {
		startToEnd[pos] = programsEnd[prog]
	}

	programRef := getPrograms(size)
	temp := make([]rune, size)
	memo := make(map[string]string)
	for i := 0; i < iterations; i++ {
		mapValues(programRef, &temp, startToEnd, memo)
	}
	return string(*programRef)
}

func mapValues(input *[]rune, temp *[]rune, mapping map[int]int, memo map[string]string) {
	if seen, ok := memo[string(*input)]; ok {
		*input = []rune(seen)
		return
	}
	length := len(*input)
	for i := 0; i < length; i++ {
		(*temp)[i] = (*input)[mapping[i]]
	}
	t := *input
	*input = *temp
	*temp = t
	memo[string(t)] = string(*input)
}

func buildMap(input []rune) map[rune]int {
	mp := make(map[rune]int)
	for i, r := range input {
		mp[r] = i
	}
	return mp
}

func getPrograms(number int) *[]rune {
	programs := make([]rune, number)
	for i := 0; i < number; i++ {
		programs[i] = rune(int('a') + i)
	}
	return &programs
}

func parseInput(input string) <-chan func(*[]rune) {
	ch := make(chan func(*[]rune))
	go func() {
		instructions := strings.Split(input, ",")
		counter := 0
		for _, instruction := range instructions {
			i := rune(instruction[0])
			switch i {
			case 's':
				number := parseSpin(instruction)
				ch <- func(x *[]rune) { spin(x, number) }
				break
			case 'x':
				a, b := parseExchange(instruction)
				ch <- func(x *[]rune) { exchange(x, a, b) }
				break
			case 'p':
				a, b := parsePartner(instruction)
				ch <- func(x *[]rune) { partner(x, a, b) }
				break
			}
			counter++
		}
		println(counter)
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

func spin(input *[]rune, number int) {
	end := (*input)[len(*input)-number:]
	start := (*input)[:len(*input)-number]
	*input = append(end, start...)
}

func exchange(input *[]rune, a int, b int) {
	pA := (*input)[a]
	(*input)[a] = (*input)[b]
	(*input)[b] = pA
}

func partner(input *[]rune, a rune, b rune) {
	var aI int
	var bI int
	for i, char := range *input {
		if char == a {
			aI = i
		}
		if char == b {
			bI = i
		}
	}
	exchange(input, aI, bI)
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
