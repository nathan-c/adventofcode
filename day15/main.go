package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

type unitType rune

var (
	elf    unitType = 'E'
	goblin unitType = 'G'
	wall   unitType = '#'
	cavern unitType = '.'
)

type unit struct {
	t   unitType
	hp  int
	pow int
}

func main() {
	m, l := parseInput("input.txt", 3)
	fmt.Println(m)

	part1(m, l)
	//part2(4)
}

func part1(units unitMap, max location) (outcome int) {
	i := 0
	fmt.Println(i)
	fmt.Println(units)

	for !runTurn(units, max) {
		i++
		// if i%10 == 0 {
		// fmt.Println(i)
		// fmt.Print(units)

		// }
	}

	fmt.Println(units)
	hpSum := 0
	for _, u := range units {
		if u.t != wall {
			hpSum += u.hp
		}
	}
	fmt.Printf("Outcome: %v * %v = %v\n", i, hpSum, hpSum*i)
	return hpSum * i
}

func part2(pow int) (outcome int) {
	units, max := parseInput("input.txt", pow)
	i := 0
	fmt.Println(i)
	fmt.Println(units)

	for {
		elfDied, gameOver := runTurnPart2(units, max)
		if elfDied {
			return part2(pow + 1)
		}
		if gameOver {
			break
		}
		// if i%10 == 0 {
		// fmt.Println(i)
		// fmt.Print(units)

		// }
	}

	fmt.Println(units)
	hpSum := 0
	for _, u := range units {
		if u.t != wall {
			hpSum += u.hp
		}
	}
	fmt.Printf("Outcome (elf strength) %v: %v * %v = %v\n", pow, i, hpSum, hpSum*i)
	return hpSum * i
}

func runTurn(units unitMap, max location) (gameOver bool) {
	moved := make(map[*unit]struct{})

	for y := 0; y <= max.y; y++ {
		for x := 0; x <= max.x; x++ {
			l := newLocation(x, y)
			if unit, ok := units[l]; ok && (unit.t == elf || unit.t == goblin) {
				if _, ok := moved[unit]; ok {
					continue
				}

				shortestPathLength, nextStepLocation, targetLoc, target, gameOver := findTarget(units, l, unit)

				if gameOver {
					return true
				}

				if target == nil {
					continue
				}

				if shortestPathLength == 0 {
					units.attack(targetLoc, target, 3)
				} else {
					delete(units, l)
					units[nextStepLocation] = unit
					if shortestPathLength == 1 {
						units.attack(targetLoc, target, 3)
					}
				}
				// mark unit as moved if a unit has moved OR attacked
				moved[unit] = struct{}{}
			}
		}
	}

	return false
}

func runTurnPart2(units unitMap, max location) (elfDied bool, gameOver bool) {
	moved := make(map[*unit]struct{})

	for y := 0; y <= max.y; y++ {
		for x := 0; x <= max.x; x++ {
			l := newLocation(x, y)
			if unit, ok := units[l]; ok && (unit.t == elf || unit.t == goblin) {
				if _, ok := moved[unit]; ok {
					continue
				}

				shortestPathLength, nextStepLocation, targetLoc, target, gameOver := findTarget(units, l, unit)

				if gameOver {
					return false, true
				}

				if target == nil {
					continue
				}

				if shortestPathLength == 0 {
					killed := units.attack(targetLoc, target, unit.pow)
					if killed && target.t == elf {
						return true, false
					}
				} else {
					delete(units, l)
					units[nextStepLocation] = unit
					if shortestPathLength == 1 {
						killed := units.attack(targetLoc, target, unit.pow)
						if killed && target.t == elf {
							return true, false
						}
					}
				}
				// mark unit as moved if a unit has moved OR attacked
				moved[unit] = struct{}{}
			}
		}
	}

	return false, false
}

func findTarget(units unitMap, l location, unit *unit) (shortestPathLength int, firstStep location, targetLocation location, target *unit, gameOver bool) {
	// Each unit begins its turn by identifying all possible targets (enemy units).
	// If no targets remain, combat ends.
	ts := units.findTargets(unit)

	if len(ts) == 0 {
		return 0, newLocation(0, 0), newLocation(0, 0), nil, true
	}

	for tl, tu := range ts {
		// If the unit is already in range of a target, it does not move,
		// but continues its turn with an attack.
		if tl.isInRange(l) &&
			(target == nil ||
				tu.hp < target.hp ||
				tu.hp == target.hp &&
					tl.Less(targetLocation)) {
			target = tu
			targetLocation = tl
		}
	}

	if target != nil {
		return
	}
	var targetOpenSquare location

	for tl, tu := range ts {
		// Otherwise, since it is not in range of a target, it moves.
		// the unit identifies all of the open squares (.) that are in range of each target;
		// these are the squares which are adjacent (immediately up, down, left, or right) to any
		// target and which aren't already occupied by a wall or another unit.
		openSquares := units.openSquaresInRangeOf(tl)

		for _, openSquare := range openSquares {
			// the unit first considers the squares that are in range
			// and determines which of those squares it could reach in the fewest steps.
			step, pathLength := units.shortestPath(l, openSquare)
			if pathLength == 0 {
				continue
			}

			if target == nil || pathLength < shortestPathLength {
				shortestPathLength = pathLength
				target = tu
				targetLocation = tl
				targetOpenSquare = openSquare
				firstStep = *step
			} else if pathLength == shortestPathLength {
				if openSquare == targetOpenSquare {
					if tu.hp < target.hp {
						target = tu
						targetLocation = tl
					}
					if step.Less(firstStep) {
						// If multiple steps would put the unit equally closer to the same target square,
						// the unit chooses the step which is first in reading order.
						firstStep = *step
					}
				} else if openSquare.Less(targetOpenSquare) {
					// If multiple squares are in range and tied for being reachable in the fewest steps,
					// the square which is first in reading order is chosen.
					target = tu
					targetLocation = tl
					targetOpenSquare = openSquare
					firstStep = *step
				}
			}
		}
		// If the unit is not already in range of a target,
		// and there are no open squares which are in range of a target, the unit ends its turn.
	}
	return
}

func parseInput(fileName string, pow int) (unitMap, location) {
	f, err := os.Open(fileName)
	if err != nil {
		log.Panicf("could not open file. %v", err)
	}
	defer f.Close()
	scanner := bufio.NewScanner(f)
	units := make(unitMap)
	y := 0
	maxx := 0
	for scanner.Scan() {
		line := []rune(scanner.Text())
		for x, ut := range line {
			if unitType(ut) == cavern {
				continue
			}
			u := &unit{unitType(ut), 200, pow}
			units[newLocation(x, y)] = u
			if x > maxx {
				maxx = x
			}
		}
		y++
	}
	return units, newLocation(maxx, y)
}
