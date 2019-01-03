package main

import (
	"fmt"
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

func (m unitMap) openSquaresInRangeOf(l location) []location {
	openSquares := make([]location, 0, 4)

	check := func(l location) {
		if _, ok := m[l]; !ok {
			openSquares = append(openSquares, l)
		}
	}

	check(newLocation(l.x-1, l.y))
	check(newLocation(l.x+1, l.y))
	check(newLocation(l.x, l.y-1))
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
