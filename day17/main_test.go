package main

import (
	"os"
	"strings"
	"testing"
)

var t1 = `
.....+......
...........#
#..#.......#
#..#..#.....
#..#..#.....
#.....#.....
#.....#.....
#######.....
............
............
...#.....#..
...#.....#..
...#.....#..
...#######..
`
var t2 = `
.+....
......
#.....
#...#.
#.#.#.
#...#.
#####.
`

var t3 = `
.............+..............
.#..........................
.#.......................#..
.#.......................#..
.#.......................#..
.#.......................#..
.#.......................#..
.#....#########..........#..
.#....#.......#..........#..
.#....#.......#..........#..
.#....#########..........#..
.#.......................#..
.#.......................#..
.#.......................#..
.#########################..
`

var t3Out = `
.............+..............
.#|||||||||||||||||||||||||.
.#~~~~~~~~~~~~~~~~~~~~~~~#|.
.#~~~~~~~~~~~~~~~~~~~~~~~#|.
.#~~~~~~~~~~~~~~~~~~~~~~~#|.
.#~~~~~~~~~~~~~~~~~~~~~~~#|.
.#~~~~~~~~~~~~~~~~~~~~~~~#|.
.#~~~~#########~~~~~~~~~~#|.
.#~~~~#.......#~~~~~~~~~~#|.
.#~~~~#.......#~~~~~~~~~~#|.
.#~~~~#########~~~~~~~~~~#|.
.#~~~~~~~~~~~~~~~~~~~~~~~#|.
.#~~~~~~~~~~~~~~~~~~~~~~~#|.
.#~~~~~~~~~~~~~~~~~~~~~~~#|.
.#########################|.
`
var t4 = `
..+....
.......
#......
#....#.
#.##.#.
#....#.
######.
`
var t4Out = `
..+....
..|....
#||||||
#~~~~#|
#~##~#|
#~~~~#|
######|
`

func Test_pour(t *testing.T) {
	tests := []struct {
		name    string
		gString string
		want    int
	}{
		{"example", t1, 57},
		{"floating", t2, 17},
	}
	for _, tt := range tests {
		g := fromDiagram(tt.gString)
		t.Run(tt.name, func(t *testing.T) {
			pour(g, g.spring.x, g.spring.y)
			if got := g.countWater(); got != tt.want {
				t.Errorf("pour() then countWater() = %v, want %v.\n%v", got, tt.want, g)
			}
		})
	}
}

func Test_pour2(t *testing.T) {
	tests := []struct {
		name    string
		gString string
		gWant   string
	}{
		{"t3", t3, t3Out},
		{"t4", t4, t4Out},
	}
	for _, tt := range tests {
		g := fromDiagram(tt.gString)
		want := fromDiagram(tt.gWant)
		t.Run(tt.name, func(t *testing.T) {
			pour(g, g.spring.x, g.spring.y)
			if got := g.countWater(); got != want.countWater() {
				t.Errorf("pour() then countWater() = %v, want %v", got, want.countWater())
			}
		})
	}
}

func fromDiagram(diag string) *grid {
	lines := strings.Split(strings.Trim(diag, "\n "), "\n")
	var g *grid
	for i, x := range []cellType(lines[0]) {
		if x == waterSpring {
			g = newGrid(i, 0)
		}
	}
	for y, line := range lines {
		for x, t := range []cellType(line) {
			if t == empty {
				continue
			}
			g.set(x, y, t)
		}
	}
	return g
}

func Benchmark_main(b *testing.B) {
	f, _ := os.Open("input.txt")
	defer f.Close()
	g := parseInput(f)

	for n := 0; n < b.N; n++ {
		pour(g, g.spring.x, g.spring.y)
	}
}

// $ go test -bench=. -cpuprofile=cpu.out
// $ go test -bench=. -blockprofile=block.out
// $ go test -bench=. -memprofile=mem.out
