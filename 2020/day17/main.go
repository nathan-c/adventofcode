package main

import (
	"bufio"
	"fmt"
	"os"
)

type pointMap struct {
	points     map[string]bool
	mins, maxs []int
}

func (p *pointMap) addPoint(coord []int, active bool) {
	p.points[fmt.Sprint(coord)] = active
}

func (p *pointMap) getPoint(coord []int) bool {
	return p.points[fmt.Sprint(coord)]
}

func (p *pointMap) countActive() int {
	count := 0
	for _, active := range p.points {
		if active {
			count++
		}
	}
	return count
}

func main() {
	pointMap3 := parseInput(3)

	for i := 0; i < 6; i++ {
		pointMap3 = runOne(pointMap3)
	}

	fmt.Printf("Part 1: %v\n", pointMap3.countActive())

	pointGrid := parseInputToGrid()

	for i := 0; i < 6; i++ {
		//pointGrid.countActive()
		//pointGrid.print()
		pointGrid = runTwo(pointGrid)
	}
	fmt.Printf("Part 2: %v\n", pointGrid.countActive())
}

func runOne(points pointMap) pointMap {
	newPoints := pointMap{points: make(map[string]bool)}

	for x := points.mins[0] - 1; x < points.maxs[0]+2; x++ {
		for y := points.mins[1] - 1; y < points.maxs[1]+2; y++ {
			for z := points.mins[2] - 1; z < points.maxs[2]+2; z++ {
				coord := c(x, y, z)
				active := points.getPoint(coord)
				activeNeighbours := 0
				for x := -1; x < 2; x++ {
					for y := -1; y < 2; y++ {
						for z := -1; z < 2; z++ {
							if x == 0 && y == 0 && z == 0 {
								continue
							}
							if points.getPoint(c(coord[0]+x, coord[1]+y, coord[2]+z)) {
								activeNeighbours++
							}
						}
					}
				}
				if active && (activeNeighbours == 2 || activeNeighbours == 3) {
					newPoints.addPoint(coord, true)
				} else if !active && activeNeighbours == 3 {
					newPoints.addPoint(coord, true)
				}
			}
		}
	}
	mins := []int{points.mins[0] - 1, points.mins[1] - 1, points.mins[2] - 1}
	maxs := []int{points.maxs[0] + 1, points.maxs[1] + 1, points.maxs[2] + 1}
	newPoints.maxs = maxs
	newPoints.mins = mins
	return newPoints
}

func runTwo(points pointGrid) pointGrid {
	newPoints := newPointGrid(points.size+2, points.offset+1)

	for x := points.mins[0] - 1; x < points.maxs[0]+2; x++ {
		for y := points.mins[1] - 1; y < points.maxs[1]+2; y++ {
			for z := points.mins[2] - 1; z < points.maxs[2]+2; z++ {
				for w := points.mins[3] - 1; w < points.maxs[3]+2; w++ {
					coord := c(x, y, z, w)
					active := points.getPoint(coord)
					activeNeighbours := 0
					for x := -1; x < 2; x++ {
						for y := -1; y < 2; y++ {
							for z := -1; z < 2; z++ {
								for w := -1; w < 2; w++ {
									if x == 0 && y == 0 && z == 0 && w == 0 {
										continue
									}
									if points.getPoint(c(coord[0]+x, coord[1]+y, coord[2]+z, coord[3]+w)) {
										activeNeighbours++
									}
								}
							}
						}
					}
					if active && (activeNeighbours == 2 || activeNeighbours == 3) {
						newPoints.addPoint(coord)
					} else if !active && activeNeighbours == 3 {
						newPoints.addPoint(coord)
					}
				}
			}
		}
	}
	mins := vectorAdd(points.mins, -1)
	maxs := vectorAdd(points.maxs, 1)
	newPoints.maxs = maxs
	newPoints.mins = mins
	return newPoints
}

func vectorAdd(vector []int, k int) []int {
	result := make([]int, len(vector))
	for i, x := range vector {
		result[i] = x + k
	}
	return result
}

func c(x ...int) []int {
	return x
}

func parseInput(dims int) pointMap {
	f, _ := os.Open("input")
	defer f.Close()

	var points [][]int

	scanner := bufio.NewScanner(f)
	y := 0
	for scanner.Scan() {
		//lines = append(lines, scanner.Text())
		for x, c := range scanner.Text() {
			if c == '#' {
				coord := make([]int, dims)
				coord[0], coord[1] = x, y
				points = append(points, coord)
			}
		}
		y++
	}

	tree := createPointMap(points)

	return tree
}
func parseInputToGrid() pointGrid {
	f, _ := os.Open("input")
	defer f.Close()

	var points [][]int

	scanner := bufio.NewScanner(f)
	y := 0
	for scanner.Scan() {
		//lines = append(lines, scanner.Text())
		for x, c := range scanner.Text() {
			if c == '#' {
				coord := make([]int, 4)
				coord[0], coord[1] = x, y
				points = append(points, coord)
			}
		}
		y++
	}

	tree := createPointGrid(points)

	return tree
}

func createPointMap(points [][]int) pointMap {
	m := pointMap{points: make(map[string]bool)}
	for _, point := range points {
		m.addPoint(point, true)
	}
	mins, maxs := bounds(points)
	m.mins = mins
	m.maxs = maxs
	return m
}

type pointGrid struct {
	points     []bool
	mins, maxs []int
	size       int
	offset     int
}

func (g *pointGrid) getPoint(coord []int) bool {
	index := g.getIndex(coord)
	if index == -1 {
		return false
	}
	return g.points[index]
}

func intPow(x, k int) int {
	result := 1
	for i := 0; i < k; i++ {
		result *= x
	}
	return result
}

func (g *pointGrid) getIndex(coord []int) int {
	size := g.size
	index := 0
	for i := 0; i < len(coord); i++ {
		k := (coord[i] + g.offset)
		if k < 0 || k >= size {
			return -1
		}
		index += k * intPow(size, len(coord)-1-i)
	}
	return index
}

func (g *pointGrid) addPoint(coord []int) {
	index := g.getIndex(coord)
	if index == -1 {
		panic(coord)
	}
	g.points[index] = true
}

func (g *pointGrid) countActive() int {

	count := 0
	for _, active := range g.points {
		if active {
			count++
		}
	}
	return count
}

func (g *pointGrid) print() {
	for w := g.mins[3]; w < g.maxs[3]+1; w++ {
		for z := g.mins[2]; z < g.maxs[2]+1; z++ {
			fmt.Printf("\nz=%v, w=%v\n", z, w)
			for y := g.mins[1]; y < g.maxs[1]+1; y++ {
				for x := g.mins[0]; x < g.maxs[0]+1; x++ {
					if g.getPoint(c(x, y, z, w)) {
						fmt.Print("#")
					} else {
						fmt.Print(".")
					}
				}
				fmt.Println()
			}
		}
	}
}

func newPointGrid(size, offset int) pointGrid {
	g := pointGrid{points: make([]bool, intPow(size, 4)), size: size, offset: offset}
	// for x := 0; x < size; x++ {
	// 	g.points[x] = make([][][]bool, size)
	// 	for y := 0; y < size; y++ {
	// 		g.points[x][y] = make([][]bool, size)
	// 		for z := 0; z < size; z++ {
	// 			g.points[x][y][z] = make([]bool, size)
	// 		}
	// 	}
	// }
	return g
}

func createPointGrid(points [][]int) pointGrid {
	mins, maxs := bounds(points)
	offset := 0
	size := 0
	for i := 0; i < len(mins); i++ {
		x := maxs[i] - mins[i] + 1
		if x > size {
			size = x
		}
		if mins[i] < offset {
			offset = mins[i]
		}
	}
	offset = -offset
	size += 2 * offset

	m := newPointGrid(size, offset)
	for _, point := range points {
		m.addPoint(point)
	}

	m.mins = vectorAdd(mins, offset)
	m.maxs = vectorAdd(maxs, offset)
	return m
}

func bounds(points [][]int) ([]int, []int) {
	mins := make([]int, len(points[0]))
	copy(mins, points[0])
	maxs := make([]int, len(points[0]))
	copy(maxs, points[0])
	for _, coord := range points {
		for axis := 0; axis < 3; axis++ {
			if coord[axis] < mins[axis] {
				mins[axis] = coord[axis]
			}
			if coord[axis] > maxs[axis] {
				maxs[axis] = coord[axis]
			}
		}
	}
	return mins, maxs
}
