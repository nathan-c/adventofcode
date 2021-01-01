package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
)

func main() {
	input := readInput()
	sort.Ints(input)
	input = append([]int{0}, input...)
	input = append(input, input[len(input)-1]+3)
	counts := make([]int, 3)
	diffs := make([]int, len(input)-1)
	for i := 1; i < len(input); i++ {
		diff := input[i] - input[i-1]
		counts[diff-1]++
		diffs[i-1] = diff
	}
	part1 := counts[0] * counts[2]
	fmt.Printf("Part 1: %v\n", part1)

	part2 := traverse(input)
	fmt.Printf("Part 2: %v\n", part2)
}

var memo map[string]int = make(map[string]int)

func traverse(input []int) int {
	key := join(input)

	if x, ok := memo[key]; ok {
		return x
	}

	if len(input) == 1 {
		memo[key] = 1
		return 1
	}
	start := input[0]
	count := 0
	for i := 1; i < len(input) && input[i]-start <= 3; i++ {
		count += traverse(input[i:])
	}
	memo[key] = count
	return count
}

func join(input []int) string {
	result := ""
	for _, x := range input {
		result += fmt.Sprint(x)
	}
	return result
}

func max(a, b int) int {
	if a > b {
		return a
	} else {
		return b
	}
}

func readInput() []int {

	f, _ := os.Open("input_test")
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
