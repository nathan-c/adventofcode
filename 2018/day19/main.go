package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

func main() {
	run(make([]int, 6, 6))
	part2 := make([]int, 6, 6)
	part2[0] = 1
	run(part2)
}

// i tried for a while to optimise the hot loops but kept introducing bugs.
// instead i outputted the state on instructions not in the hot loop to see if i could see a pattern
// i noticed that register[0] was sum of common divisors
func run(register []int) {
	instructions := buildInstructionMap()
	program, ipRegister := parseInput(instructions)
	hotLoopPrint := 0
	ip := 0
	for {
		register[ipRegister] = ip

		if ip == 3 {
			divisors := getCommonDivisors(register[4])
			dSum := 0
			for _, i := range divisors {
				dSum += i
			}
			fmt.Println(dSum)
			return
		} else if ip < len(program) && ip >= 0 {
			inst := program[ip]
			inst.i(register, inst.a, inst.b, inst.c)
		} else {
			break
		}
		if ip != 3 && ip != 4 && ip != 5 && ip != 6 && ip != 8 && ip != 9 && ip != 10 && ip != 11 {
			// print out all out of sequence instructions (the hot loop consists of 3,4,5,6,8,9,10,11)
			// this will remove a lot of cruft from the output and give us a better view of how the program runs.
			//fmt.Printf("%v\t%v\t%v\n", hotLoopPrint, ip, register)
		}
		ip = register[ipRegister]
		ip++
		hotLoopPrint++
	}
	fmt.Println(register[0])
}

func getCommonDivisors(x int) []int {
	var divisors []int
	sqrt := int(math.Sqrt(float64(x))) + 1

	for i := 1; i < sqrt; i++ {
		if x%i == 0 && i*i != x {
			divisors = append(divisors, i, x/i)
		} else if x%i == 0 && i*i == x {
			divisors = append(divisors, i)
		}
	}
	return divisors
}

type instruction struct {
	i       func([]int, int, int, int)
	a, b, c int
}

func parseInput(instructionSet map[string]func([]int, int, int, int)) ([]instruction, int) {
	f, err := os.Open("input.txt")
	if err != nil {
		log.Panicf("could not open file. %v", err)
	}
	defer f.Close()
	scanner := bufio.NewScanner(f)
	scanner.Scan()
	ip, _ := strconv.Atoi(scanner.Text()[4:])
	var program []instruction
	for scanner.Scan() {
		line := strings.Split(scanner.Text(), " ")
		inst, _ := instructionSet[line[0]]
		a, _ := strconv.Atoi(line[1])
		b, _ := strconv.Atoi(line[2])
		c, _ := strconv.Atoi(line[3])
		program = append(program, instruction{inst, a, b, c})
	}
	return program, ip
}
