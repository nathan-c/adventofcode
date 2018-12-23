package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type acreType int

func (a acreType) String() string {
	switch a {
	case open:
		return "."
	case trees:
		return "|"
	case lumber:
		return "#"
	}
	return strconv.Itoa(int(a))
}

func (a acreType) cycle() acreType {
	if a == lumber {
		return open
	}
	return a + 1
}

func runeToAcreType(r rune) acreType {
	switch r {
	case '.':
		return open
	case '|':
		return trees
	case '#':
		return lumber
	}
	log.Panicf("invalid input %v", r)
	return -1
}

const (
	open acreType = iota
	trees
	lumber
)

type acres [][]acreType

func newAcres(rowCount, colCount int) acres {
	var a acres = make([][]acreType, rowCount, rowCount)
	for i := 0; i < rowCount; i++ {
		a[i] = make([]acreType, colCount, colCount)
	}
	return a
}

func (a acres) rowCount() int {
	return len(a)
}
func (a acres) colCount() int {
	return len(a[0])
}

func (a acres) addRow(row int, s []rune) {
	for i, r := range s {
		a[row][i] = runeToAcreType(r)
	}
}

func (a acres) nextMinute() acres {
	newA := newAcres(a.rowCount(), a.colCount())
	return a.nextMinuteWithoutAlloc(newA)
}

func (a acres) nextMinuteWithoutAlloc(newA acres) acres {
	for rowNo, row := range a {
		for colNo, currentType := range row {
			nextType := currentType.cycle()
			count := a.countSurrounding(rowNo, colNo, nextType)
			treesOrLumber := func() {
				if count > 2 {
					newA[rowNo][colNo] = nextType
				} else {
					newA[rowNo][colNo] = currentType
				}
			}
			switch nextType {
			case trees:
				treesOrLumber()
			case lumber:
				treesOrLumber()
			case open:
				treeCount := a.countSurrounding(rowNo, colNo, trees)
				lumberCount := a.countSurrounding(rowNo, colNo, lumber)
				if lumberCount > 0 && treeCount > 0 {
					newA[rowNo][colNo] = lumber
				} else {
					newA[rowNo][colNo] = open
				}
			}
		}
	}
	return newA
}

func (a acres) countSurrounding(rowNo, colNo int, t acreType) int {
	minRow, maxRow, minCol, maxCol := a.validNeighbours(rowNo, colNo)
	i := 0
	for row := minRow; row <= maxRow; row++ {
		for col := minCol; col <= maxCol; col++ {
			if row == rowNo && col == colNo {
				continue
			}
			if a[row][col] == t {
				i++
			}
		}
	}

	return i
}

type coord struct {
	rowNo, colNo int
}

func (a acres) validNeighbours(rowNo, colNo int) (minRow, maxRow, minCol, maxCol int) {

	minRow = rowNo
	if rowNo > 0 {
		minRow = rowNo - 1
	}
	minCol = colNo
	if colNo > 0 {
		minCol = colNo - 1
	}
	maxRow = rowNo
	if rowNo < len(a)-1 {
		maxRow = rowNo + 1
	}
	maxCol = colNo
	if colNo < len(a[0])-1 {
		maxCol = colNo + 1
	}
	return
}

func (a acres) score() int {
	treeCount := 0
	lumberCount := 0
	for _, row := range a {
		for _, currentType := range row {
			if currentType == trees {
				treeCount++
			}
			if currentType == lumber {
				lumberCount++
			}
		}
	}
	return treeCount * lumberCount
}

func (a acres) String() string {
	var output strings.Builder
	for _, row := range a {
		for _, currentType := range row {
			output.WriteString(currentType.String())
		}
		output.WriteRune('\n')
	}
	return output.String()
}

func main() {
	part1()
	part2()
}

func part1() {
	a := parseInput()

	for i := 0; i < 10; i++ {
		a = a.nextMinute()
	}

	fmt.Printf("part one \n%v\n", a)
	fmt.Printf("score: %v\n", a.score())
}

func part2() {
	a1 := parseInput()
	a2 := newAcres(a1.rowCount(), a1.colCount())
	seenScores := make(map[int]int)
	targetCycles := 1000000000
	cycleLength := 0
	cycleStart := 0

	for i := 0; i < targetCycles; i++ {
		a1, a2 = a1.nextMinuteWithoutAlloc(a2), a1

		if i > 1000 {
			if x, ok := seenScores[a1.score()]; ok {
				cycleStart = i
				cycleLength = i - x
				break
			} else {
				seenScores[a1.score()] = i
			}

		}
	}
	numCycles := (targetCycles - cycleStart) / cycleLength
	mod := targetCycles - (cycleStart + numCycles*cycleLength) - 1

	for i := 0; i < mod; i++ {
		a1, a2 = a1.nextMinuteWithoutAlloc(a2), a1
	}
	fmt.Printf("part two\n%v\n", a1)
	fmt.Printf("score: %v\n", a1.score())
}

func parseInput() acres {
	f, err := os.Open("input.txt")
	if err != nil {
		log.Panicf("could not open file. %v", err)
	}
	defer f.Close()
	scanner := bufio.NewScanner(f)
	rowNo := 0
	a := newAcres(50, 50)
	for scanner.Scan() {
		a.addRow(rowNo, []rune(scanner.Text()))
		rowNo++
	}
	return a
}

func parseLine() {}
