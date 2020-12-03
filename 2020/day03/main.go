package main

import (
	"bufio"
	"log"
	"os"
)

func main() {
	file, err := os.Open("input")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var input [][]bool

	for scanner.Scan() {
		row := fromString(scanner.Text())
		input = append(input, row)
	}

	log.Printf("Part 1 trees hit: %v", processSlope(input, 3, 1))

	result := processSlope(input, 1, 1)
	result = result * processSlope(input, 3, 1)
	result = result * processSlope(input, 5, 1)
	result = result * processSlope(input, 7, 1)
	result = result * processSlope(input, 1, 2)

	log.Printf("Part 2: %v", result)
}

func processSlope(input [][]bool, xStep, yStep int) int {

	xLen := len(input[0])

	count := 0
	for x, y := 0, 0; y < len(input); x, y = x+xStep, y+yStep {
		if input[y][x%xLen] {
			count++
		}
	}

	return count
}

func fromString(input string) []bool {

	var row []bool

	for _, x := range input {
		row = append(row, x == '#')
	}

	return row
}
