package main

import (
	"fmt"
	"strconv"
	"strings"
)

func main() {
	//input, last := parseInput("0,12,6,13,20,1,17")
	input, last := parseInput("0,3,6")

	for i := len(input); i < 30000000; i++ {
		seen := input[last]
		if len(seen) == 1 {
			last = 0
		} else {
			last = seen[1] - seen[0]
		}
		if seen, ok := input[last]; ok {
			seen = append(seen, i)
			if len(seen) > 2 {
				seen = seen[len(seen)-2:]
			}
			input[last] = seen
		} else {
			input[last] = []int{i}
		}
	}

	fmt.Printf("Part 1: %v\n", last)
}

func parseInput(input string) (map[int][]int, int) {
	split := strings.Split(input, ",")
	ints := make(map[int][]int)
	last := 0
	for j, x := range split {
		i, err := strconv.Atoi(x)
		if err != nil {
			panic(err)
		}
		ints[i] = []int{j}
		last = i
	}
	return ints, last
}
