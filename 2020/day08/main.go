package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	input := readInput()
	trace := make(map[int]bool)
	i := 0
	acc := 0
	for {
		if _, ok := trace[i]; ok {
			fmt.Printf("Part 1: %v\n", acc)
			break
		}
		trace[i] = true

		op := input[i]
		if op.op == "acc" {
			acc += op.arg
			i++
		} else if op.op == "jmp" {
			i += op.arg
		} else {
			i++
		}
	}

	for i, v := range input {
		if v.op == "jmp" || v.op == "nop" {
			clone := make([]instruction, len(input))
			copy(clone, input)
			if v.op == "jmp" {
				clone[i].op = "nop"
			} else {
				clone[i].op = "jmp"
			}
			if pass, thisAcc := run(clone); pass {
				fmt.Printf("Part 2: %v", thisAcc)
				return
			}
		}
	}
}

func run(input []instruction) (bool, int) {
	trace := make(map[int]bool)
	i := 0
	acc := 0
	for {
		if _, ok := trace[i]; ok {
			return false, acc
		}
		trace[i] = true

		op := input[i]
		if op.op == "acc" {
			acc += op.arg
			i++
		} else if op.op == "jmp" {
			i += op.arg
		} else {
			i++
		}
		if i == len(input) {
			return true, acc
		}
	}
}

func readInput() []instruction {

	f, _ := os.Open("input")
	defer f.Close()

	var input []instruction

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		var op string
		var arg int
		fmt.Sscanf(scanner.Text(), "%s %d", &op, &arg)
		input = append(input, instruction{op: op, arg: arg})
	}

	return input
}

type instruction struct {
	op  string
	arg int
}
