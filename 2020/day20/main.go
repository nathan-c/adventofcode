package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"regexp"
	"sort"
	"strconv"
)

type tile struct {
	id       int
	edges    []string
	lines    []string
	rotation int
	flip     bool
}

func main() {
	tiles := readInput()
	// tile := &tile{lines: []string{"12", "43"}, edges: getEdges([]string{"12", "43"})}
	// fmt.Println(tile.edges)

	// fmt.Print(tile)
	// //fmt.Println(tile.rotation, "\t", tile.flip, "\t", tile.getEdge(0), tile.getEdge(1), tile.getEdge(2), tile.getEdge(3))
	// for i := 1; i < 8; i++ {
	// 	tile.incrementTransform()
	// 	//fmt.Println(tile.rotation, "\t", tile.flip, "\t", tile.getEdge(0), tile.getEdge(1), tile.getEdge(2), tile.getEdge(3))
	// 	fmt.Print(tile)
	// }

	result := partOne(tiles)
	fmt.Printf("Part 1: %v\n", result)
	partTwo(tiles)
}

func partOne(tiles []tile) int {
	edges := make(map[string][]int)

	for _, tile := range tiles {
		for _, edge := range tile.edges {
			if ids, ok := edges[edge]; ok {
				edges[edge] = append(ids, tile.id)
			} else if ids, ok := edges[reverse(edge)]; ok {
				edges[reverse(edge)] = append(ids, tile.id)
			} else {
				edges[edge] = []int{tile.id}
			}
		}
	}

	noMatches := make(map[int]int)
	for _, ids := range edges {
		if len(ids) == 1 {
			noMatches[ids[0]]++
		}
	}

	count := 0
	result := 1
	for id, matchCount := range noMatches {
		if matchCount == 2 {
			count++
			result *= id
		}
	}
	if count != 4 {
		panic("err")
	}
	return result
}

type seam struct {
	tileID int
	edge   string
}

type tuple struct {
	tileID int
	seams  []*seam
}

type edgeToTileMap map[string][]int

type fullArray struct {
	x        []string
	rotation int
	flip     bool
}

func (a *fullArray) getRow(y int) string {
	l := len(a.x)
	row := ""
	for x := 0; x < l; x++ {
		xP, yP := transform(x, y, l-1, a.rotation, a.flip)
		row += string(a.x[yP][xP])
	}
	return row
}

func (m edgeToTileMap) get(edge string) []int {
	ids := m[edge]
	if len(ids) != 0 {
		return ids
	}
	return m[reverse(edge)]
}

func partTwo(tiles []tile) {
	tileMap := make(map[int]*tile)

	edges := make(edgeToTileMap)

	for _, tile := range tiles {
		x := tile
		tileMap[tile.id] = &x
		for _, edge := range tile.edges {
			if ids, ok := edges[edge]; ok {
				edges[edge] = append(ids, tile.id)
			} else if ids, ok := edges[reverse(edge)]; ok {
				edges[reverse(edge)] = append(ids, tile.id)
			} else {
				edges[edge] = []int{tile.id}
			}
		}
	}

	neighbourMap := make(map[int][]*seam)
	for _, tile := range tiles {
		for _, edge := range tile.edges {
			tileIds := edges.get(edge)
			for _, tileID := range tileIds {
				if tileID == tile.id {
					continue
				}
				neighbourMap[tile.id] = append(neighbourMap[tile.id], &seam{tileID, edge})
			}
		}
	}

	orderedNeighbours := make([]tuple, len(neighbourMap))
	i := 0
	for id, neighbours := range neighbourMap {
		//fmt.Println(id, neighbours)
		orderedNeighbours[i] = tuple{id, neighbours}
		i++
	}
	sort.SliceStable(orderedNeighbours, func(i, j int) bool {
		return len(orderedNeighbours[i].seams) < len(orderedNeighbours[j].seams)
	})

	// for _, x := range orderedNeighbours {
	// 	fmt.Println(x)
	// }

	size := int(math.Sqrt(float64(len(tiles))))
	tileArray := make([][]*tile, size)

	// top right bottom left
	tlNeighbours := orderedNeighbours[0]
	tl := tileMap[tlNeighbours.tileID]
	//top and left should not be seams

	for len(edges.get(tl.getEdge(0))) != 1 ||
		len(edges.get(tl.getEdge(3))) != 1 {
		tl.incrementTransform()
	}

	tileArray[0] = make([]*tile, size)
	tileArray[0][0] = tl
	// set top row
	for x := 1; x < size; x++ {
		prev := tileArray[0][x-1]
		prevEdge := prev.getEdge(1)
		var this *tile
		for _, seam := range neighbourMap[prev.id] {
			if seam.edge == prevEdge || reverse(seam.edge) == prevEdge {
				this = tileMap[seam.tileID]
				break
			}
		}
		if this == nil {
			panic("err")
		}
		for this.getEdge(3) != prevEdge {
			this.incrementTransform()
		}
		tileArray[0][x] = this
		neighbourMap[prev.id] = removeTile(neighbourMap[prev.id], this.id)
		neighbourMap[this.id] = removeTile(neighbourMap[this.id], prev.id)
	}

	// set left col
	for y := 1; y < size; y++ {
		tileArray[y] = make([]*tile, size)
		prev := tileArray[y-1][0]
		prevEdge := prev.getEdge(2)
		var this *tile
		for _, seam := range neighbourMap[prev.id] {
			if seam.edge == prevEdge || reverse(seam.edge) == prevEdge {
				this = tileMap[seam.tileID]
				break
			}
		}
		if this == nil {
			panic("err")
		}
		for this.getEdge(0) != prevEdge {
			this.incrementTransform()
		}
		tileArray[y][0] = this
		neighbourMap[prev.id] = removeTile(neighbourMap[prev.id], this.id)
		neighbourMap[this.id] = removeTile(neighbourMap[this.id], prev.id)
	}

	// do other rows
	for y := 1; y < size; y++ {
		for x := 1; x < size; x++ {
			t := tileArray[y-1][x]
			l := tileArray[y][x-1]
			te := t.getEdge(2)
			le := l.getEdge(1)
			tn := neighbourMap[t.id]
			ln := neighbourMap[l.id]
			if len(tn) != 1 {
				panic("oh noooo")
			}
			// if len(ln) != 1 {
			// 	panic("oh noooo")
			// }
			if !contains(ln, tn[0].tileID) {
				panic("grrrr")
			}
			tile := tileMap[tn[0].tileID]
			for tile.getEdge(0) != te {
				tile.incrementTransform()
			}
			if tile.getEdge(3) != le {
				panic("oops")
			}
			tileArray[y][x] = tile
			neighbourMap[tile.id] = removeTile(neighbourMap[tile.id], t.id, l.id)
			neighbourMap[t.id] = removeTile(neighbourMap[t.id], tile.id)
			neighbourMap[l.id] = removeTile(neighbourMap[l.id], tile.id)
		}
	}
	array := buildArray(tileArray)

	monster0, _ := regexp.Compile(`..................#.`) //1
	monster1, _ := regexp.Compile(`#....##....##....###`) //8
	monster2, _ := regexp.Compile(`.#..#..#..#..#..#...`) //6
	//monster == 15#

	var monsters [][]int
	possibles := 0
	miss := 0
	matchCount := 0
	hashCount := 0
	for matchCount == 0 {
		hashCount = 0
		for y := 0; y < len(array.x); y++ {
			row := array.getRow(y)
			for _, x := range row {
				if x == '#' {
					hashCount++
				}
			}
			if y == 0 || y == len(array.x)-1 {
				continue
			}

			idx := 0
			match := monster1.FindStringIndex(row[idx:])
			for match != nil {
				possibles++
				prevRow := array.getRow(y - 1)
				if monster0.MatchString(prevRow[idx+match[0] : idx+match[1]]) {
					nextRow := array.getRow(y + 1)
					if monster2.MatchString(nextRow[idx+match[0] : idx+match[1]]) {
						monsters = append(monsters, []int{y, idx + match[0], idx + match[1]})
						matchCount++
						idx += match[1]
					} else {
						miss++
						idx += match[0] + 1
					}
				} else {
					miss++
					idx += match[0] + 1
				}
				match = monster1.FindStringIndex(row[idx:])
			}
		}
		if matchCount == 0 {
			array.incrementTransform()
		}
	}
	printArray(array)

	hashCount2 := 0
	for y := 0; y < len(array.x); y++ {
		for x := 0; x < len(array.x[0]); x++ {
			if array.x[y][x] == '#' {
				hashCount2++
			}
		}
	}
	fmt.Printf("Part 2: %v\n", hashCount-(matchCount*15)) //1695 is too high
	for _, monster := range monsters {
		fmt.Println(monster)
	}

}

func printArray(array *fullArray) {
	for y := 0; y < len(array.x); y++ {
		fmt.Println(array.getRow(y))
	}
}

func (t *tile) String() string {
	l := len(t.edges[0])
	sb := make([]rune, 0, l*l+l)
	for y := 0; y < l; y++ {
		for x := 0; x < l; x++ {
			sb = append(sb, t.get(x, y))
		}
		sb = append(sb, '\n')
	}
	sb = append(sb, '\n')
	return string(sb)
}

func buildArray(arrayTiles [][]*tile) *fullArray {
	xTile := len(arrayTiles[0][0].lines[0])
	yTile := len(arrayTiles[0][0].lines)
	xLen := len(arrayTiles[0]) * (xTile - 2)
	yLen := len(arrayTiles) * (yTile - 2)

	array := make([]string, yLen)
	for y := 0; y < yLen; y++ {
		array[y] = ""
		j := y / (len(arrayTiles[0][0].lines) - 2)
		jOff := y - (j * (yTile - 2)) + 1
		for x := 0; x < xLen; x++ {
			i := x / (len(arrayTiles[0][0].lines[0]) - 2)
			iOff := x - (i * (xTile - 2)) + 1
			tile := arrayTiles[j][i]
			array[y] += string(tile.get(iOff, jOff))
		}
	}
	return &fullArray{x: array}
}

func contains(slice []*seam, tileID int) bool {
	for _, x := range slice {
		if x.tileID == tileID {
			return true
		}
	}
	return false
}

func removeTile(slice []*seam, tileIDs ...int) []*seam {
	newSlice := make([]*seam, 0, len(slice))
	for _, x := range slice {
		skip := false
		for _, id := range tileIDs {
			if x.tileID == id {
				skip = true
			}
		}
		if skip {
			continue
		}
		newSlice = append(newSlice, x)
	}
	return newSlice
}

func reverse(x string) string {
	runes := make([]rune, len(x))
	for i, r := range []rune(x) {
		runes[len(runes)-1-i] = r
	}
	return string(runes)
}

func match(a, b string) bool {
	if a == b {
		return true
	}
	if reverse(a) == b {
		return true
	}
	return false
}

func readInput() []tile {

	f, _ := os.Open("input")
	defer f.Close()

	var tiles []tile

	var currentID int
	var currentTile []string

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			t := tile{id: currentID, edges: getEdges(currentTile), lines: currentTile}
			tiles = append(tiles, t)
			currentID = 0
			currentTile = nil
			continue
		}
		if line[:5] == "Tile " {
			id, err := strconv.Atoi(line[5 : len(line)-1])
			if err != nil {
				panic(err)
			}
			currentID = id
		} else {
			currentTile = append(currentTile, line)
		}
	}
	if len(currentTile) > 1 {
		t := tile{id: currentID, edges: getEdges(currentTile), lines: currentTile}
		tiles = append(tiles, t)
	}
	return tiles
}

func getEdges(tile []string) []string {
	var edges []string
	left := ""
	right := ""
	for _, line := range tile {
		left = string(line[0]) + left
		right += string(line[len(line)-1])
	}
	edges = append(edges, tile[0], right, reverse(tile[len(tile)-1]), left)
	return edges
}

// should always return in left to right top to bottom order to make matching easier
func (t *tile) getEdge(idx int) string {
	shouldReverse := false
	realIdx := (idx - t.rotation + 4) % 4
	if t.flip {
		if realIdx == 1 || realIdx == 3 {
			realIdx = (realIdx + 2) % 4
		}
		shouldReverse = true
	}
	val := t.edges[realIdx]
	if idx > 1 {
		shouldReverse = !shouldReverse
	}

	if shouldReverse {
		return reverse(val)
	}
	return val
}

// should always return in left to right top to bottom order to make matching easier
func (t *tile) get(x, y int) rune {
	l := len(t.edges[0]) - 1
	if t.rotation == 1 {
		x, y = y, l-x
	} else if t.rotation == 2 {
		x, y = l-x, l-y
	} else if t.rotation == 3 {
		x, y = l-y, x
	}
	if t.flip {
		x = l - x
	}
	return rune(t.lines[y][x])
}

func transform(x, y, l, r int, flip bool) (xP int, yP int) {

	if r == 1 {
		x, y = y, l-x
	} else if r == 2 {
		x, y = l-x, l-y
	} else if r == 3 {
		x, y = l-y, x
	}
	if flip {
		x = l - x
	}
	return x, y
}

func (t *tile) incrementTransform() {
	if t.rotation == 3 {
		if t.flip {
			panic("done all transforms")
		}
		t.flip = true
		t.rotation = 0
	} else {
		t.rotation++
	}
}
func (a *fullArray) incrementTransform() {
	if a.rotation == 3 {
		if a.flip {
			panic("done all transforms")
		}
		a.flip = true
		a.rotation = 0
	} else {
		a.rotation++
	}
}

func (s *seam) String() string {
	return fmt.Sprint(s.tileID, s.edge)
}

func (t tuple) String() string {
	return fmt.Sprint(t.tileID, t.seams)
}
