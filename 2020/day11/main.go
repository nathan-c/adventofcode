package main

import (
	"bufio"
	"fmt"
	"os"
)

type seat rune

var empty seat = 'L'
var occupied seat = '#'
var floor seat = '.'

type seats [][]seat

func main() {
	input := readInput()
	//input.print()
	next := input
	changed := true
	for changed {
		next, changed = next.runPartOneRound()
		//next.print()
	}
	//next.print()
	part1 := next.countOccupied()
	fmt.Printf("Part 1: %v\n", part1)

	next = input
	changed = true
	for changed {
		next, changed = next.runPartTwoRound()
		//next.print()
	}
	//next.print()
	part2 := next.countOccupied()
	fmt.Printf("Part 2: %v\n", part2)
}

func readInput() seats {

	f, _ := os.Open("input")
	defer f.Close()

	var input seats

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		runes := []seat(scanner.Text())

		input = append(input, runes)
	}

	return input
}

func (input seats) runPartOneRound() (seats, bool) {
	var output seats
	changed := false
	for y := 0; y < len(input); y++ {
		var row []seat
		for x := 0; x < len(input[0]); x++ {
			oldSeat := input[y][x]
			newSeat := input.runPartOneSeat(x, y)
			if newSeat != oldSeat {
				changed = true
			}

			row = append(row, newSeat)
		}
		output = append(output, row)
	}
	return output, changed
}

func (input seats) runPartOneSeat(x, y int) seat {
	seat := input[y][x]
	if seat.isFloor() {
		return '.'
	}
	occupiedCount := 0
	for i := x - 1; i <= x+1; i++ {
		for j := y - 1; j <= y+1; j++ {
			if i == x && j == y {
				continue
			}
			if c, ok := input.get(i, j); ok {
				if c.isOccupied() {
					occupiedCount++
				}
			}
		}
	}
	if seat.isEmpty() && occupiedCount == 0 {
		return occupied
	}
	if seat.isOccupied() && occupiedCount > 3 {
		return empty
	}
	return seat
}
func (input seats) runPartTwoRound() (seats, bool) {
	var output seats
	changed := false
	for y := 0; y < len(input); y++ {
		var row []seat
		for x := 0; x < len(input[0]); x++ {
			oldSeat := input[y][x]
			newSeat := input.runPartTwoSeat(x, y)
			if newSeat != oldSeat {
				changed = true
			}

			row = append(row, newSeat)
		}
		output = append(output, row)
	}
	return output, changed
}

func (input seats) runPartTwoSeat(x, y int) seat {
	seat := input[y][x]
	if seat.isFloor() {
		return '.'
	}
	occupiedCount := 0
	for i := -1; i <= 1; i++ {
		for j := -1; j <= 1; j++ {
			if i == 0 && j == 0 {
				continue
			}
			if probe(input, x, y, i, j) {
				occupiedCount++
			}

		}
	}
	if seat.isEmpty() && occupiedCount == 0 {
		return occupied
	}
	if seat.isOccupied() && occupiedCount > 4 {
		return empty
	}
	return seat
}

func (input seats) get(x, y int) (seat, bool) {
	if y < 0 || y >= len(input) || x < 0 || x >= len(input[0]) {
		return '\000', false
	}
	return input[y][x], true
}

func (s seat) isEmpty() bool {
	if s == empty {
		return true
	}
	return false
}

func (s seat) isOccupied() bool {
	if s == occupied {
		return true
	}
	return false
}
func (s seat) isFloor() bool {
	if s == floor {
		return true
	}
	return false
}
func (input seats) countOccupied() int {
	count := 0
	for _, row := range input {
		for _, seat := range row {
			if seat.isOccupied() {
				count++
			}
		}
	}
	return count
}
func (input seats) print() {
	for _, row := range input {
		fmt.Println(rowToString(row))
	}
	fmt.Println()
}
func (s seat) String() string {
	return string(s)
}
func rowToString(row []seat) string {
	str := ""
	for _, s := range row {
		str += s.String()
	}
	return str
}
func probe(input seats, x, y, i, j int) bool {
	x += i
	y += j
	for x >= 0 && x < len(input[0]) && y >= 0 && y < len(input) {
		if input[y][x].isFloor() {
			x += i
			y += j
		} else {
			return input[y][x].isOccupied()
		}
	}
	return false
}
