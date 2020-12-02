package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

type entry struct {
	min, max int
	letter   rune
	password string
}

func main() {
	file, err := os.Open("input")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	validCount := 0
	valid2Count := 0

	for scanner.Scan() {
		row := fromString(scanner.Text())
		if row.isValidPart1() {
			validCount++
		}
		if row.isValidPart2() {
			valid2Count++
		}
	}
	log.Printf("Valid Part 1: %v\n", validCount)
	log.Printf("Valid Part 2: %v\n", valid2Count)

}

func fromString(input string) entry {
	var min int
	var max int
	var letter rune
	var password string

	fmt.Sscanf(input, "%d-%d %c: %s", &min, &max, &letter, &password)
	return entry{min, max, letter, password}
}

func (e entry) isValidPart1() bool {
	count := 0
	for _, c := range e.password {
		if c == e.letter {
			count++
		}
	}

	if count <= e.max && count >= e.min {
		return true
	}
	return false
}

func (e entry) isValidPart2() bool {
	passwordRunes := []rune(e.password)

	return (passwordRunes[e.min-1] == e.letter && passwordRunes[e.max-1] != e.letter) ||
		(passwordRunes[e.min-1] != e.letter && passwordRunes[e.max-1] == e.letter)
}
