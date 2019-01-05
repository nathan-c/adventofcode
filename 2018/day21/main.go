package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func main() {
	register := make([]int, 6, 6)
	run(register)
}

func run(register []int) {
	instructions := buildInstructionMap()
	program, ipRegister := parseInput(instructions)
	loopCount := 0
	ip := 0
	reg3 := make(map[int]int)
	mostRecentReg3 := 0
	for {
		register[ipRegister] = ip

		if ip < len(program) && ip >= 0 {
			inst := program[ip]
			inst.i(register, inst.a, inst.b, inst.c)
		} else {
			break
		}

		ip = register[ipRegister]

		if ip == 28 {
			// line 28 is the only line that uses register 0
			// the value of register[3] when we first hit line 28 is
			// the solution to part 1.
			// for part 2, monitor register[3] values on line 28 for a loop
			// when we start to loop take the previous value as
			// this is the last one before we start repeating ourselves!
			if len(reg3) == 0 {
				fmt.Printf("part 1: %v\n", register[3])
			}
			if _, ok := reg3[register[3]]; !ok {
				reg3[register[3]] = loopCount
				mostRecentReg3 = register[3]
			} else {
				fmt.Printf("part 2: %v\n", mostRecentReg3)
				break
			}
		}
		ip++
		loopCount++
	}
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
