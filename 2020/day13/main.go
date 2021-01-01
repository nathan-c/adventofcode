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
		if bus == -1 {
			continue
		}
		next := (timestamp/bus)*bus + bus
		if next < earliest {
			earliest = next
			busID = bus
		}
	}

	fmt.Printf("Part 1: %v\n", busID*(earliest-timestamp))

	fmt.Printf("Part 2: %v\n", part2_3(getFastBusses(busses)))
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

func part2_2(busses []int) int {

	maxI, maxBus := max(busses)
	fastBusses := getFastBusses(busses)

	for target := (100000000000000 / maxBus) * maxBus; ; target = target + maxBus {
		skip := false
		for _, bus := range fastBusses {
			if bus.index == maxI {
				continue
			}
			if (target-maxI+bus.index)%bus.bus != 0 {
				skip = true
				break
			}
		}
		if !skip {
			return target - maxI
		}
		if target+maxBus < target {
			panic("err")
		}
	}
}

func part2_3(input []tuple) int {

	var as []int
	var ns []int
	for _, t := range input {
		as = append(as, mod(-t.index, t.bus))
		ns = append(ns, t.bus)
	}

	return crt(ns, as)
}

func crt(ns, as []int) int {

	p := 1
	for _, n := range ns {
		p *= n
	}

	x := 0
	for i, n := range ns {
		a := as[i]
		q := p / n
		x += a * mulInv(q, n) * q
	}

	return mod(x, p)
}

func mulInv(a, b int) int {

	b0 := b
	x0, x1 := 0, 1
	if b == 1 {
		return 1
	}
	for a > 1 {
		q := a / b
		a, b = b, mod(a, b)
		x0, x1 = x1-q*x0, x0
	}
	if x1 < 0 {
		x1 += b0
	}
	return x1
}

func mod(a, b int) int {
	x := a % b
	if x < 0 {
		return x + b
	}
	return x
}

type tuple struct {
	index, bus int
}

func getFastBusses(busses []int) []tuple {
	var result []tuple
	for i, bus := range busses {
		if bus == -1 {
			continue
		}
		result = append(result, tuple{i, bus})
	}
	//sort.SliceStable(result, func(i, j int) bool { return result[i].bus > result[j].bus })
	return result
}

func max(ints []int) (int, int) {
	maxI := 0
	max := ints[0]
	for i, x := range ints {
		if x > max {
			maxI = i
			max = x
		}
	}
	return maxI, max
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

	f, _ := os.Open("input")
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
