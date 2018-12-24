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
	run(make([]int, 6, 6))
	part2 := make([]int, 6, 6)
	part2[0] = 1
	//run(part2)
}

func run(register []int) {
	instructions := buildInstructionMap()
	program, ipRegister := parseInput(instructions)
	programHitCount := make([]int, len(program), len(program))
	hotLoopPrint := 0
	ip := 0
	// reg4 := 0
	for {
		register[ipRegister] = ip

		// if ip == 9 && register[2] < register[4] {
		// 	//shortcut!
		// 	register[2] = register[4] + 1
		// 	register[1] = 1
		// } else if ip == 13 && register[5] < register[4] {
		// 	register[1] = 1
		// 	register[5] = register[4] + 1
		// } else
		if ip < len(program) && ip >= 0 {
			programHitCount[ip]++
			inst := program[ip]
			inst.i(register, inst.a, inst.b, inst.c)
		} else {
			break
		}
		ip = register[ipRegister]
		ip++
		// if register[4] != reg4 {
		// 	fmt.Printf("%v %v %v\n", hotLoopPrint, register, programHitCount)
		// 	reg4 = register[4]
		// }
		fmt.Printf("%v %v %v\n", hotLoopPrint, register, programHitCount)
		hotLoopPrint++
		// if hotLoopPrint < 253234000 && hotLoopPrint > 253233000 {
		// 	fmt.Printf("%v %v\n", register, programHitCount)
		// 	//hotLoopPrint = 0
		// }
	}
	fmt.Println(register[0])
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
