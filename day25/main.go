package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"

	"github.com/kyroy/kdtree"
)

type point struct {
	x, y, z, t int
}

// implementation of go-kdtree.Point interface
func (p *point) Dimensions() int {
	return 4
}

// implementation of go-kdtree.Point interface
func (p *point) Dimension(dim int) float64 {
	switch dim {
	case 0:
		return float64(p.x)
	case 1:
		return float64(p.y)
	case 2:
		return float64(p.z)
	case 3:
		return float64(p.t)
	default:
		panic(fmt.Sprint("invalid dimension ", dim))
	}
}

// implementation of go-kdtree.Point interface
func (p *point) Distance(other interface{}) float64 {
	var ret float64
	for i := 0; i < p.Dimensions(); i++ {
		tmp := math.Abs(p.Dimension(i) - other.(*point).Dimension(i))
		ret += tmp
	}
	return ret
}

// implementation of go-kdtree.Point interface
func (p *point) PlaneDistance(val float64, dim int) float64 {
	tmp := math.Abs(p.Dimension(dim) - val)
	return tmp
}

type constellation struct {
	points []*point
}

func (c *constellation) String() string {
	var sb strings.Builder
	sb.WriteString("[")
	for _, p := range c.points {
		sb.WriteString(fmt.Sprint(*p, ", "))
	}
	sb.WriteString("]")
	return sb.String()
}

func newConstellation(root *point) *constellation {
	return &constellation{[]*point{root}}
}

func (c *constellation) add(p *point) bool {
	add := false
	for _, pt := range c.points {
		if dist(pt, p) < 4 {
			add = true
			break
		}
	}
	if add {
		c.points = append(c.points, p)
	}
	return add
}

func (c *constellation) squash(c2 *constellation) bool {
	squash := false
top:
	for _, pt1 := range c.points {
		for _, pt2 := range c2.points {
			if dist(pt1, pt2) < 4 {
				squash = true
				break top
			}
		}
	}

	if squash {
		c.points = append(c.points, c2.points...)
	}
	return squash
}

func (c *constellation) canSquash(c2 *constellation) bool {
	for _, pt1 := range c.points {
		for _, pt2 := range c2.points {
			if dist(pt1, pt2) < 4 {
				return true
			}
		}
	}
	return false
}

func dist(a, b *point) int {
	return abs(a.x-b.x) + abs(a.y-b.y) + abs(a.z-b.z) + abs(a.t-b.t)
}

func abs(a int) int {
	if a < 0 {
		return -a
	}
	return a
}

func main() {
	points := parseInput("test.txt")
	fmt.Printf("part one: %v constellations", x(points))
}

func x(points []*point) int {
	uf := newUnionFind(len(points))
	toI := make(map[*point]int)
	for i, p1 := range points {
		toI[p1] = i
		for p2, j := range toI {
			if dist(p1, p2) <= 3 {
				uf.merge(i, j)
			}
		}
	}
	return uf.numSets
}

func part1(points []*point) int {
	checkForDups(points)
	constellations := buildUsingTree(points)
	sum := 0
	for _, c := range constellations {
		sum += len(c.points)
	}
	if sum != len(points) {
		panic("uh oh! invalid constellations")
	}

	for i := 0; i < len(constellations); i++ {
		for j := i + 1; j < len(constellations); j++ {
			c1 := constellations[i]
			c2 := constellations[j]
			if c1.canSquash(c2) {
				fmt.Printf("can squash %[1]v with %[2]v\n%[1]v: %[3]v\n%[2]v: %[4]v\n\n", i, j, c1, c2)
			}
		}
	}

	return len(constellations)
}

func checkForDups(points []*point) {
	seen := make(map[point]struct{})
	for i, pt := range points {
		if _, ok := seen[*pt]; ok {
			panic(fmt.Sprintf("dup at %v", i))
		}
		seen[*pt] = struct{}{}
	}
}

func formConstellations(points []*point) []*constellation {
	constellations := []*constellation{}

	for len(points) > 0 {
		pt := points[0]
		points = points[1:]
		added := false
		for _, c := range constellations {
			if c.add(pt) {
				added = true
				break
			}
		}
		if !added {
			constellations = append(constellations, newConstellation(pt))
		}
	}

	constellations = squashConstellations(constellations)
	return constellations
}

func squashConstellations(constellations []*constellation) []*constellation {
	for i := len(constellations) - 1; i > -1; i-- {
		for j := 0; j < i; j++ {
			if constellations[j].squash(constellations[i]) {
				constellations = constellations[:len(constellations)-1]
			}
		}
	}
	return constellations
}

func buildUsingTree(points []*point) []*constellation {
	kdPoints := make([]kdtree.Point, len(points))
	for i := 0; i < len(points); i++ {
		kdPoints[i] = points[i]
	}

	tree := kdtree.New(kdPoints)

	constellations := []*constellation{}

	type queueItem struct {
		point kdtree.Point
	}
	queue := []queueItem{}

	for pts := tree.Points(); len(pts) > 0; pts = tree.Points() {
		pt := pts[0]
		tree.Remove(pt)
		tree.Balance() // i found that there is a bug in the kd-tree implementation i chose so i needs rebuilding after every delete :(. If i get time i will take a look
		constellation := newConstellation(pt.(*point))
		constellations = append(constellations, constellation)
		if len(constellations) == 221 {

		}
		queue = append(queue, queueItem{pt})
		for len(queue) > 0 {
			item := queue[0]
			queue = queue[1:]

			for {
				neighbours := tree.KNN(item.point, 1)
				if len(constellations) == 221 {
					fmt.Printf("%v\n", tree)
				}
				if len(neighbours) == 0 {
					break
				}
				pt2 := neighbours[0]
				if pt2.Distance(item.point) > 3 {
					//fmt.Printf("%v -> %v is too far: %v\n", item.point, pt2, pt2.Distance(item.point))
					break
				} else {
					constellation.points = append(constellation.points, pt2.(*point))
					tree.Remove(pt2)
					tree.Balance() // i found that there is a bug in the kd-tree implementation i chose so i needs rebuilding after every delete :(. If i get time i will take a look
					queue = append(queue, queueItem{pt2})
				}
			}
		}
	}
	return constellations
}

func parseInput(fileName string) []*point {

	f, err := os.Open(fileName)
	if err != nil {
		log.Panicf("could not open file. %v", err)
	}
	defer f.Close()
	atoi := func(x string) int {
		i, _ := strconv.Atoi(x)
		return i
	}
	scanner := bufio.NewScanner(f)
	var points []*point
	for scanner.Scan() {
		splitLine := strings.Split(scanner.Text(), ",")
		pt := &point{
			x: atoi(splitLine[0]),
			y: atoi(splitLine[1]),
			z: atoi(splitLine[2]),
			t: atoi(splitLine[3]),
		}
		points = append(points, pt)
	}
	return points
}
