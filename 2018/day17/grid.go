package main

import (
	"bufio"
	"io"
	"math"
	"regexp"
	"strconv"
	"strings"
)

type cellType rune

var (
	empty         cellType = '.'
	clay          cellType = '#'
	runningWater  cellType = '|'
	standingWater cellType = '~'
	waterSpring   cellType = '+'
)

type location struct {
	x, y int
}

type grid struct {
	cells  map[location]cellType
	spring location
	top    int
	bottom int
}

func newGrid(x, y int) *grid {
	m := make(map[location]cellType)
	m[location{x, y}] = waterSpring
	return &grid{m, location{x, y}, math.MaxInt64, math.MinInt64}
}

func (g *grid) get(x, y int) cellType {
	if t, ok := g.cells[location{x, y}]; ok {
		return t
	}
	return empty
}

func (g *grid) set(x, y int, t cellType) {
	g.cells[location{x, y}] = t

	if t == clay {
		if y < g.top {
			g.top = y
		}
		if y > g.bottom {
			g.bottom = y
		}
	}
}

func (g *grid) isClayOrStanding(x, y int) bool {
	t := g.get(x, y)
	return t == clay || t == standingWater
}

func (g *grid) countWater() int {
	miny := g.top
	sum := 0
	for l, v := range g.cells {
		if v == standingWater || v == runningWater && l.y >= miny {
			sum++
		}
	}
	return sum
}

func (g *grid) countStanding() int {
	miny := g.top
	sum := 0
	for l, v := range g.cells {
		if v == standingWater && l.y >= miny {
			sum++
		}
	}
	return sum
}

func (g *grid) String() string {
	minx := math.MaxInt32
	maxx := math.MinInt32
	maxy := math.MinInt32
	for k := range g.cells {
		if k.x < minx {
			minx = k.x
		}
		if k.x > maxx {
			maxx = k.x
		}
		if k.y > maxy {
			maxy = k.y
		}
	}
	var sb strings.Builder
	for y := 0; y <= maxy; y++ {
		if sb.Len() != 0 {
			sb.WriteRune('\n')
		}
		for x := minx; x <= maxx; x++ {
			sb.WriteRune(rune(g.get(x, y)))
		}
	}
	return sb.String()
}

func parseInput(r io.Reader) *grid {
	scanner := bufio.NewScanner(r)
	g := newGrid(500, 0)

	atoi := func(x string) int {
		i, _ := strconv.Atoi(x)
		return i
	}

	rxStr := `(x|y)=(\d+), (x|y)=(\d+)..(\d+)`
	rx := regexp.MustCompile(rxStr)
	for scanner.Scan() {
		captures := rx.FindStringSubmatch(scanner.Text())
		fst := captures[1]
		fstVal := atoi(captures[2])
		snd := captures[3]
		sndStart := atoi(captures[4])
		sndEnd := atoi(captures[5])
		for i := sndStart; i <= sndEnd; i++ {
			if fst == "x" {
				g.set(fstVal, i, clay)
			} else if snd == "x" {
				g.set(i, fstVal, clay)
			}
		}
	}

	return g
}
