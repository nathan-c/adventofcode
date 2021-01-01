package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {

	f, _ := os.Open("input")
	defer f.Close()

	scanner := bufio.NewScanner(f)
	scanner.Split(scanDoubleLines)

	part1 := 0
	part2 := 0

	for scanner.Scan() {
		answers := make(map[rune]int)
		personCount := 1
		for i, c := range scanner.Text() {
			if c >= 'a' && c <= 'z' {
				answers[c]++
			}
			if c == '\n' && i != 0 {
				personCount++
			}
		}
		part1 += len(answers)
		for _, v := range answers {
			if v == personCount {
				part2++
			}
		}
	}

	fmt.Printf("Part 1: %v\n", part1)

	fmt.Printf("Part 2: %v\n", part2)
}

func scanDoubleLines(data []byte, atEOF bool) (advance int, token []byte, err error) {
	if atEOF && len(data) == 0 {
		return 0, nil, nil
	}

	if i := strings.Index(string(data), "\n\n"); i >= 0 {
		// We have a full newline-terminated line.
		return i + 1, data[0:i], nil
	}
	// If we're at EOF, we have a final, non-terminated line. Return it.
	if atEOF {
		return len(data), data, nil
	}
	// Request more data.
	return 0, nil, nil
}
