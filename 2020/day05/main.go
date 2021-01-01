package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"sort"
)

func main() {

	f, _ := os.Open("input")
	defer f.Close()

	scanner := bufio.NewScanner(f)

	max := 0
	var seats []int
	rows := 0

	for scanner.Scan() {
		rows++
		ticket := []rune(scanner.Text())
		row := getSeat(ticket[:7])
		column := getSeat(ticket[7:])
		id := (row * 8) + column
		fmt.Printf("%s, row: %v, seat: %v, id: %v\n", string(ticket), row, column, id)
		seats = append(seats, id)
		if max < id {
			max = id
		}
	}

	fmt.Printf("max seat id: %v", max)

	sort.Ints(seats)

	for i := 1; i < len(seats); i++ {
		plus1 := seats[i]
		minus1 := seats[i-1]
		if plus1-minus1 == 2 {
			fmt.Printf("my seat: %v\n", minus1+1)
		}
	}
}

func getSeat(row []rune) int {
	min := 0
	max := int(math.Pow(2, float64(len(row))))
	for _, c := range row {
		if c == 'F' || c == 'L' {
			max = max - (max-min)/2
		}
		if c == 'B' || c == 'R' {
			min = min + (max-min)/2
		}
	}
	return min
}
