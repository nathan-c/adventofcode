package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

type state struct {
	direction, x, y int
}

func main() {
	ship := state{direction: 90, x: 0, y: 0}
	instructions := readInput()
	for _, instruction := range instructions {
		ship = move1(instruction, ship)
	}

	fmt.Printf("Part 1: %v\n", abs(ship.x)+abs(ship.y))

	ship = state{direction: 0, x: 0, y: 0}
	waypoint := state{direction: 0, x: 10, y: 1}
	for _, instruction := range instructions {
		ship, waypoint = move2(instruction, ship, waypoint)
	}

	fmt.Printf("Part 2: %v\n", abs(ship.x)+abs(ship.y))

}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func mod(x, i int) int {
	y := x - (x/i)*i
	if y < 0 {
		return i + y
	}
	return y
}

func readInput() []string {

	f, _ := os.Open("input")
	defer f.Close()

	var input []string

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		input = append(input, scanner.Text())
	}

	return input
}

func move1(instruction string, ship state) state {
	number, err := strconv.Atoi(instruction[1:])
	if err != nil {
		panic(err)
	}
	if instruction[0] == 'N' {
		return state{ship.direction, ship.x, ship.y + number}
	}
	if instruction[0] == 'S' {
		return state{ship.direction, ship.x, ship.y - number}
	}
	if instruction[0] == 'E' {
		return state{ship.direction, ship.x + number, ship.y}
	}
	if instruction[0] == 'W' {
		return state{ship.direction, ship.x - number, ship.y}
	}
	if instruction[0] == 'L' {
		return state{ship.direction - number, ship.x, ship.y}
	}
	if instruction[0] == 'R' {
		return state{ship.direction + number, ship.x, ship.y}
	}
	if instruction[0] == 'F' {
		direction := mod(ship.direction, 360)
		if direction == 0 {
			return move1(fmt.Sprintf("N%v", number), ship)
		}
		if direction == 90 {
			return move1(fmt.Sprintf("E%v", number), ship)
		}
		if direction == 180 {
			return move1(fmt.Sprintf("S%v", number), ship)
		}
		if direction == 270 {
			return move1(fmt.Sprintf("W%v", number), ship)
		}
	}
	panic(fmt.Sprintf("%v %v", instruction, ship))
}

func move2(instruction string, ship, waypoint state) (state, state) {
	number, err := strconv.Atoi(instruction[1:])
	if err != nil {
		panic(err)
	}
	if instruction[0] == 'N' {
		return ship, state{0, waypoint.x, waypoint.y + number}
	}
	if instruction[0] == 'S' {
		return ship, state{0, waypoint.x, waypoint.y - number}
	}
	if instruction[0] == 'E' {
		return ship, state{0, waypoint.x + number, waypoint.y}
	}
	if instruction[0] == 'W' {
		return ship, state{0, waypoint.x - number, waypoint.y}
	}
	if instruction[0] == 'L' || instruction[0] == 'R' {
		rotation := number
		if instruction[0] == 'L' {
			rotation = -rotation
		}
		rotation = mod(rotation, 360)
		if rotation == 0 {
			return ship, waypoint
		}
		if rotation == 90 {
			return ship, state{0, waypoint.y, -waypoint.x}
		}
		if rotation == 180 {
			return ship, state{0, -waypoint.x, -waypoint.y}
		}
		if rotation == 270 {
			return ship, state{0, -waypoint.y, waypoint.x}
		}
	}
	if instruction[0] == 'F' {
		return state{0, ship.x + (waypoint.x * number), ship.y + (waypoint.y * number)}, waypoint
	}
	panic(fmt.Sprintf("%v %v", instruction, ship))
}
