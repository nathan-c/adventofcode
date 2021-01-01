package main

import (
	"bufio"
	"fmt"
	"os"
	"unicode"
)

func main() {
	input := readInput()
	part1 := partOne(input)
	fmt.Printf("Part 1: %v\n", part1)
	//part2 := partTwo([]string{"((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"})
	part2 := partTwo(input)
	fmt.Printf("Part 2: %v\n", part2)
}

func partOne(calcs []string) int {
	sum := 0
	for _, calc := range calcs {
		sum += calculateP1(calc)
	}
	return sum
}

func partTwo(calcs []string) int {
	sum := 0
	for _, calc := range calcs {
		tokens := parseP2(calc)
		sum += calculateP2(tokens)
	}
	return sum
}

func readInput() []string {
	f, _ := os.Open("input")
	defer f.Close()

	var inputs []string

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		inputs = append(inputs, line)
	}
	return inputs
}

func calculateP1(calculation string) int {
	var opened []int
	var closed []int
	value := 0
	operation := '+'
	for i, c := range calculation {
		if c == ' ' {
			continue
		}

		if len(opened) == 0 && unicode.IsDigit(c) {
			value = apply(operation, value, int(c-'0'))
		} else if c == '(' {
			opened = append(opened, i)
		} else if c == ')' {
			closed = append(closed, i)
		} else if len(opened) == 0 {
			operation = c
		}

		if len(opened) > 0 && len(opened) == len(closed) {
			subCalc := calculateP1(calculation[opened[0]+1 : closed[len(closed)-1]])
			opened, closed = nil, nil
			value = apply(operation, value, subCalc)
		}
	}
	return value
}

func calculateP2(tokens []interface{}) int {

	for i, token := range tokens {
		if subCalc, ok := token.([]interface{}); ok {
			result := calculateP2(subCalc)
			tokens[i] = result
		}
	}
	var tokens2 []int
	for i, token := range tokens {
		if op, ok := token.(op); ok {
			if op.t == '+' {
				if len(tokens2) > 0 {
					result := tokens2[len(tokens2)-1] + tokens[i+1].(int)
					tokens2 = append(tokens2[:len(tokens2)-1], result)
				} else {
					result := tokens[i-1].(int) + tokens[i+1].(int)
					tokens2 = append(tokens2, result)
				}

			} else {
				if len(tokens2) > 0 {
					tokens2 = append(tokens2, tokens[i+1].(int))
				} else {
					tokens2 = append(tokens2, tokens[i-1].(int), tokens[i+1].(int))
				}
			}
		}
	}
	retVal := 1
	for _, token := range tokens2 {
		retVal *= token
	}
	return retVal
}

type op struct {
	t rune
}

func parseP2(calculation string) []interface{} {

	var opened []int
	var closed []int
	var tokens []interface{}
	for i, c := range calculation {
		if c == ' ' {
			continue
		}

		if len(opened) == 0 && unicode.IsDigit(c) {
			tokens = append(tokens, int(c-'0'))
		} else if c == '(' {
			opened = append(opened, i)
		} else if c == ')' {
			closed = append(closed, i)
		} else if len(opened) == 0 {
			tokens = append(tokens, op{c})
		}

		if len(opened) > 0 && len(opened) == len(closed) {
			subCalc := parseP2(calculation[opened[0]+1 : closed[len(closed)-1]])
			opened, closed = nil, nil
			tokens = append(tokens, subCalc)
		}
	}
	return tokens
}

func apply(op rune, a, b int) int {
	if op == '+' {
		return a + b
	}
	if op == '*' {
		return a * b
	}
	panic(fmt.Errorf("op: %v, a: %v, b: %v", op, a, b))
}
