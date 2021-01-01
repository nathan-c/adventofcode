package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
)

var size int = 25

func main() {
	input := readInput()
	part1, part1I := partOne(input)
	fmt.Printf("Part 1: %v\n", part1)

	part2 := partTwo(part1, part1I, input)
	fmt.Printf("Part 2: %v\n", part2)

}

func partOne(input []int) (int, int) {

	for i := 0; i < len(input)-size; i++ {
		if !hasAddends(input[i+size], input[i:i+size]) {
			return input[i+size], i + size
		}
	}
	panic("oops")
}

func partTwo(part1, part1I int, input []int) int {

	for i := 0; i < part1I; i++ {
		sum := 0
		min := math.MaxInt64
		max := 0
		for j := i; j < part1I; j++ {
			val := input[j]
			sum += val
			if val > max {
				max = val
			}
			if val < min {
				min = val
			}
			if sum == part1 {
				return min + max
			}
			if sum > part1 {
				break
			}
		}
	}
	panic("oops2")
}

func hasAddends(target int, input []int) bool {
	for i, a := range input {
		for j := i + 1; j < len(input); j++ {
			if a+input[j] == target {
				return true
			}
		}
	}
	return false
}

func readInput() []int {

	f, _ := os.Open("input")
	defer f.Close()

	var input []int

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		i, err := strconv.Atoi(scanner.Text())
		if err != nil {
			panic(scanner.Text())
		}

		input = append(input, i)
	}

	return input
}
