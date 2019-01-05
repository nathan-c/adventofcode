package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"math"
	"os"
	"sort"
	"strings"
)

type unitMap map[location]*unit

func (m unitMap) findTargets(u *unit) unitMap {
	targets := make(unitMap)
	targetType := elf
	if u.t == elf {
		targetType = goblin
	}
	for k, v := range m {
		if v == u {
			continue
		}
		if v.t == targetType {
			targets[k] = v
		}
	}
	return targets
}

func (m unitMap) hasTargets(u *unit) bool {
	targetType := u.t.getEnemy()
	for _, v := range m {
		if v == u {
			continue
		}
		if v.t == targetType {
			return true
		}
	}
	return false
}

func (m unitMap) openSquaresInRangeOf(l location) []location {
	openSquares := make([]location, 0, 4)

	check := func(l location) {
		if _, ok := m[l]; !ok {
			openSquares = append(openSquares, l)
		}
	}

	check(newLocation(l.x, l.y-1))
	check(newLocation(l.x-1, l.y))
	check(newLocation(l.x+1, l.y))
	check(newLocation(l.x, l.y+1))

	return openSquares
}

func (m unitMap) attack(l location, u *unit, power int) (killed bool) {
	u.hp -= power
	if u.hp < 1 {
		delete(m, l)
		return true
	}
	return false
}

func (m unitMap) String() string {
	var sb strings.Builder
	maxx := 0
	for x := 0; ; x++ {
		if _, ok := m[newLocation(x, 0)]; !ok {
			maxx = x
			break
		}
	}
	maxy := 0
	for y := 0; ; y++ {
		if _, ok := m[newLocation(0, y)]; !ok {
			maxy = y
			break
		}
	}
	for y := 0; y < maxy; y++ {
		for x := 0; x < maxx; x++ {
			l := newLocation(x, y)
			if u, ok := m[l]; ok {
				sb.WriteRune(rune(u.t))
			} else {
				sb.WriteRune('.')
			}
		}

		// sb.WriteString("\t\t")
		sb.WriteString("      ")

		for x := 0; x < maxx; x++ {
			l := newLocation(x, y)
			if u, ok := m[l]; ok {
				if u.t == elf || u.t == goblin {
					// sb.WriteString(fmt.Sprintf("%c(%v), ", u.t, u.hp))
					sb.WriteString(fmt.Sprintf("%v ", u.hp))
				}
			}
		}

		sb.WriteRune('\n')
	}
	return sb.String()
}

func (m unitMap) isInRange(l location, t unitType) bool {

	test := func(l location) bool {
		u, ok := m[l]
		return ok && u.t == t
	}

	if test(location{l.x - 1, l.y}) {
		return true
	}
	if test(location{l.x + 1, l.y}) {
		return true
	}
	if test(location{l.x, l.y - 1}) {
		return true
	}
	if test(location{l.x, l.y + 1}) {
		return true
	}
	return false
}

func (m unitMap) inRange(l location, t unitType) (tl location, tu *unit) {

	test := func(l location) {
		if u, ok := m[l]; ok && u.t == t {
			if tu == nil || tu.hp > u.hp {
				tu = u
				tl = l
			}
		}
	}

	test(location{l.x, l.y - 1})
	test(location{l.x - 1, l.y})
	test(location{l.x + 1, l.y})
	test(location{l.x, l.y + 1})
	return
}

type possibleMove struct {
	first     location
	length    int
	targetLoc location
}

// Perform a breadth first search for each adjacent tile
// Although not the most efficient, the approach is still fast and makes it
// easy to sort in such a way that satisfies all the conditions
func (m unitMap) findNextMove(l location, enemyType unitType) (location, bool) {

	//If an enemy is in range of our current location we are done!
	if m.isInRange(l, enemyType) {
		return l, false
	}

	//Keep the possible moves we've found
	//At the end we'll need to use all these values to find the proper move
	bestMoves := []possibleMove{}

	//We'll need to keep track of two things -
	//seenCoords - the tiles we've already visited
	//stack - the "new" tiles accessible from the current furthest points
	seenCoords := map[location]struct{}{l: struct{}{}}
	openSquares := m.openSquaresInRangeOf(l)
	stack := make([]possibleMove, len(openSquares))
	for i, s := range openSquares {
		stack[i] = possibleMove{s, 1, s}
	}

	//Now do the search -

	i := 1 //Already have moved one tile at this point
	run := true
	for run {
		i++

		//Keep track of the new tiles here
		newStack := []possibleMove{}

		//Loop through and look for new tiles to add
		for _, tile := range stack {
			if _, ok := seenCoords[tile.targetLoc]; ok {
				continue
			}

			seenCoords[tile.targetLoc] = struct{}{}

			if m.isInRange(tile.targetLoc, enemyType) {
				bestMoves = append(bestMoves, tile)
				//We want to complete this iteration to find all other reachable tiles at the same distance
				run = false
				continue
			}

			//Add all newly accessible tiles to stack
			for _, l := range m.openSquaresInRangeOf(tile.targetLoc) {
				newStack = append(newStack, possibleMove{tile.first, i, l})
			}
		}
		stack = make([]possibleMove, len(newStack))
		copy(stack, newStack)
		//We might also need to end at this point if we have no more newly accessible tiles
		if len(stack) < 1 {
			run = false
		}
	}
	//Take our list of the bestMoves from each starting point that we generated, and find the one move we'll take
	return getBestMove(bestMoves)
}

func (m unitMap) sumUnitHp() int {
	hpSum := 0
	for _, u := range m {
		if u.t != wall {
			hpSum += u.hp
		}
	}
	return hpSum
}

// Takes a list of tuples of
// (first_move, number_of_moves, tile_coordinates), which might look like -
// {{12, 22}, 8, {17, 25}}
// {{12, 22}, 8, {18, 24}}
// {{12, 22}, 8, {19, 21}}
// {{13, 21}, 6, {19, 21}}
// {{13, 23}, 6, {17, 25}}
// {{13, 23}, 6, {18, 24}}
// {{14, 22}, 6, {17, 25}}
// {{14, 22}, 6, {18, 24}}
// {{14, 22}, 6, {19, 21}}
// And filters/sorts them to satisfy all the conditions
func getBestMove(moves []possibleMove) (location, bool) {

	if moves == nil || len(moves) < 1 {
		return location{}, false
	}

	//First condition - fewest number of moves away
	minSteps := math.MaxInt64
	for _, n := range moves {
		if n.length < minSteps {
			minSteps = n.length
		}
	}
	bestMoves := make([]possibleMove, 0, 4)
	for _, n := range moves {
		if n.length == minSteps {
			bestMoves = append(bestMoves, n)
		}
	}

	//Second condition - if tie, choose the first tile in reading order
	sort.Slice(bestMoves, func(i, j int) bool {
		return bestMoves[i].targetLoc.Less(bestMoves[j].targetLoc)
	})
	tmp := make([]possibleMove, 0, len(bestMoves))
	minMove := bestMoves[0].targetLoc
	for _, n := range bestMoves {
		if n.targetLoc == minMove {
			tmp = append(tmp, n)
		}
	}
	bestMoves = tmp

	//Third condition - if tie, take the first step in reading order
	sort.Slice(bestMoves, func(i, j int) bool {
		return bestMoves[i].first.Less(bestMoves[j].first)
	})
	tmp = make([]possibleMove, 0, len(bestMoves))
	minFirst := bestMoves[0].first
	for _, n := range bestMoves {
		if n.first == minFirst {
			tmp = append(tmp, n)
		}
	}
	bestMoves = tmp

	return bestMoves[0].first, true
}

func parseInputFromString(x string, pow int) (unitMap, location) {
	return parseInputFromReader(strings.NewReader(x), pow)
}

func parseInputFromFile(fileName string, pow int) (unitMap, location) {
	f, err := os.Open(fileName)
	if err != nil {
		log.Panicf("could not open file. %v", err)
	}
	defer f.Close()
	return parseInputFromReader(f, pow)
}

func parseInputFromReader(r io.Reader, pow int) (unitMap, location) {
	scanner := bufio.NewScanner(r)
	units := make(unitMap)
	y := 0
	maxx := 0
	for scanner.Scan() {
		line := []rune(scanner.Text())
		for x, ut := range line {
			if unitType(ut) == cavern {
				continue
			}
			var u *unit
			if unitType(ut) == elf {
				u = &unit{unitType(ut), 200, pow}
			} else {
				u = &unit{unitType(ut), 200, 3}
			}
			units[newLocation(x, y)] = u
			if x > maxx {
				maxx = x
			}
		}
		y++
	}
	return units, newLocation(maxx, y)
}
