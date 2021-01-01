package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

func main() {
	timestamp, busses := readInput()
	var busID int
	earliest := math.MaxInt64
	for _, bus := range busses {
		next := (timestamp/bus)*bus + bus
		if next < earliest {
			earliest = next
			busID = bus
		}
	}

	fmt.Printf("Part 1: %v\n", busID*(earliest-timestamp))
	part2_2(busses)
	fmt.Printf("Part 2: %v\n", part2(busses))
}

func part2(busses []int) int {
	x := 0
	incrementSize := busses[0]

	for i := busses[0]; ; i += incrementSize {
		x = i
		for j, bus := range busses {
			if j == 0 || bus == -1 {
				continue
			}
			if (i+j-1)/bus != (i+j)/bus {
				//incrementSize = i
				continue
			} else {
				x = 0
				break
			}
		}
		if x != 0 {
			return x
		}
	}
}

func part2_2(busses []int) {

	divisor := busses[0]
	minTime := 0

	for i, bus := range busses {
		if bus == -1 {
			continue
		}
		for j := 1; ; j++ {
			time := j * bus
			if time < minTime {
				continue
			}
			if (time-i)%divisor == 0 {
				fmt.Println(i, bus, j, time)
				minTime = time
				break
			}
		}
	}
}

func probe(a, b, i int) int {
	if (a*i)+1 == b {
		return i
	}
	if a*i < b {
		return probe(a, b, i+1)
	}
	return -1
}

func readInput() (int, []int) {

	f, _ := os.Open("input_test")
	defer f.Close()

	var busTimes []int
	var timestamp int
	var err error

	scanner := bufio.NewScanner(f)
	scanner.Scan()
	timestamp, err = strconv.Atoi(scanner.Text())
	if err != nil {
		panic(err)
	}
	scanner.Scan()
	busses := strings.Split(scanner.Text(), ",")
	for _, bus := range busses {
		if bus == "x" {
			busTimes = append(busTimes, -1)
		} else {
			time, err := strconv.Atoi(bus)
			if err != nil {
				panic(err)
			}
			busTimes = append(busTimes, time)
		}
	}

	return timestamp, busTimes
}
