package main

import (
	"fmt"
	"io/ioutil"
)

type unitType rune

var (
	elf    unitType = 'E'
	goblin unitType = 'G'
	wall   unitType = '#'
	cavern unitType = '.'
)

func (t unitType) getEnemy() unitType {
	switch t {
	case elf:
		return goblin
	case goblin:
		return elf
	default:
		panic(fmt.Sprintf("unit %v has no enemy!", t))
	}
}

type unit struct {
	t   unitType
	hp  int
	pow int
}

func main() {
	inputBytes, _ := ioutil.ReadFile("input.txt")
	inputString := string(inputBytes)

	m, l := parseInputFromString(inputString, 3)
	part1(m, l)

	part2(inputString, 4)
}

func part1(units unitMap, max location) (outcome int) {
	i := 0

	runTurnPart1 := func() bool {
		_, end := runTurn(units, max, false)
		return !end
	}

	for runTurnPart1() {
		i++
	}

	hpSum := units.sumUnitHp()
	fmt.Printf("Outcome: %v * %v = %v\n", i, hpSum, hpSum*i)
	return hpSum * i
}

func part2(inputString string, pow int) (outcome int) {
	units, max := parseInputFromString(inputString, pow)
	i := 0
	gameOver := false
	for !gameOver {
		var elfDied bool
		elfDied, gameOver = runTurn(units, max, true)
		if elfDied {
			return part2(inputString, pow+1)
		}
		if !gameOver {
			i++
		}
	}
	hpSum := units.sumUnitHp()
	fmt.Printf("Outcome (elf strength %v): %v * %v = %v\n", pow, i, hpSum, hpSum*i)
	return hpSum * i
}

func runTurn(units unitMap, max location, exitOnElfDeath bool) (elfDied bool, gameOver bool) {
	moved := make(map[*unit]struct{})

	for y := 0; y <= max.y; y++ {
		for x := 0; x <= max.x; x++ {
			l := newLocation(x, y)
			if unit, ok := units[l]; ok && (unit.t == elf || unit.t == goblin) {
				if _, ok := moved[unit]; ok {
					continue
				}

				if !units.hasTargets(unit) {
					return false, true
				}

				step, ok := units.findNextMove(l, unit.t.getEnemy())

				if ok {
					units[step] = units[l]

					delete(units, l)
				}

				loc, target := units.inRange(step, unit.t.getEnemy())
				if target != nil {
					killed := units.attack(loc, target, unit.pow)
					if exitOnElfDeath && killed && target.t == elf {
						return true, false
					}
				}

				moved[unit] = struct{}{}
			}
		}
	}

	return false, false
}
