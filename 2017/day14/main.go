package main

import (
	"strconv"
	"fmt"
)

func main() {
	print(part2("hxtvlmkl"))
}

func part2(key string) int {
	var board [][]bool
	for i := 0; i < 128; i++ {
		keyI := fmt.Sprintf("%s-%d", key, i)
		data := knotHash(keyI)
		binary := formatOutput(data)
		board = append(board, binary)
	}
	numRegions := 0
	for r := 0; r < len(board); r++ {
		for c := 0; c < len(board[0]); c++ {
			numRegions += fillRegion(board, r, c)
		}
	}
	return numRegions
}

func knotHash(input string) []int {
	currentPosition := 0
	skipSize := 0
	data := make([]int, 256)
	for i := 0; i < 256; i++ {
		data[i] = i
	}
	lengths := getLengths(input)
	for i := 0; i < 64; i++ {
		currentPosition, skipSize, data = singleRound(lengths, currentPosition, skipSize, data)
	}
	return compressHash(data)
}

func getLengths(inputBytes string) []int {
	lengths := make([]int, len(inputBytes))
	for pos, char := range inputBytes {
		lengths[pos] = int(char)
	}
	return append(lengths, []int{17, 31, 73, 47, 23}...)
}

func singleRound(lengths []int, currentPosition int, skipSize int, data []int) (int, int, []int) {
	for _, length := range lengths {
		for i := 0; i < (length / 2); i++ {
			swap(data, currentPosition+i, currentPosition+length-i-1)
		}
		currentPosition = (currentPosition + length + skipSize) % len(data)
		skipSize++
	}
	return currentPosition, skipSize, data
}

func swap(l []int, a int, b int) {
	a = a % len(l)
	b = b % len(l)
	x := l[a]
	l[a] = l[b]
	l[b] = x
}

func compressHash(sparseHash []int) []int {
	var denseHash []int
	chunkSize := len(sparseHash) / 16
	for i := 0; i < len(sparseHash); i += chunkSize {
		element := 0
		for j := 0; j < chunkSize; j++ {
			element ^= sparseHash[i+j]
		}
		denseHash = append(denseHash, element)
	}
	return denseHash
}

func formatOutput(denseHash []int) []bool {
	intermediate1 := ""
	for _, x := range denseHash {
		intermediate1 += fmt.Sprintf("%02x", x)
	}
	intermediate2 := ""
	for _, y := range intermediate1 {
		s, _ := strconv.ParseInt(string(rune(y)), 16, 64)
		intermediate2 += fmt.Sprintf("%04b", s)
	}
	var retVal []bool
	for _, z := range intermediate2 {
		if z == '1' {
			retVal = append(retVal, true)
		} else {
			retVal = append(retVal, false)
		}
	}
	return retVal
}

type coord struct {
	row int
	col int
}

func fillRegion(board [][]bool, r int, c int) int {
	if !board[r][c] {
		return 0
	}
	var q []coord
	q = append(q, coord{row: r, col: c})
	for len(q) > 0 {
		p := q[0]
		r = p.row
		c = p.col
		q = q[1:]
		if !board[r][c] {
			continue
		}
		for _, child := range getJoining(board, r, c) {
			q = append(q, child)
		}
		board[r][c] = false
	}
	return 1
}

func getJoining(board [][]bool, row int, col int) []coord {
	cl := coord{row, col - 1}
	bc := coord{row + 1, col}
	cr := coord{row, col + 1}
	tc := coord{row - 1, col}
	possibleVals := []coord{cl, bc, cr, tc}
	height := len(board)
	width := len(board[0])
	var retVal []coord
	for _, p := range possibleVals {
		r := p.row
		c := p.col
		if r >= 0 && r < height && c >= 0 && c < width && board[r][c] {
			retVal = append(retVal, coord{r, c})
		}
	}
	return retVal
}
