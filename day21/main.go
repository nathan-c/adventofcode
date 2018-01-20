package main

import (
	"io/ioutil"
	"strings"
)

func main() {
	grid := [][]bool{{false, true, false}, {false, false, true}, {true, true, true}}
	map2, map3 := loadRulesFromFile("day21.input.txt")
	println(len(map2))
	println(len(map3))
	for i := 0; i < 5; i++ {
		print2D(grid)
		println()
		grid = runIteration(grid, map2, map3)
	}
	print2D(grid)
	println()
	println(countOn(grid))
}

func print2D(s [][]bool) {
	for _, r := range s {
		for _, cell := range r {
			if cell {
				print("#")
			} else {
				print(".")
			}
		}
		println()
	}
}

func countOn(s [][]bool) int {
	count := 0
	for _, r := range s {
		for _, item := range r {
			if item {
				count++
			}
		}
	}
	return count
}
func runIteration(grid [][]bool, map2 map[[2][2]bool][3][3]bool, map3 map[[3][3]bool][4][4]bool) [][]bool {
	var blockSize int
	if len(grid)%2 == 0 {
		blockSize = 2
	} else {
		blockSize = 3
	}
	if blockSize == 2 {
		newGrid := buildGrid(len(grid) / 2 * 3)
		for r := 0; r < len(grid); r = r + blockSize {
			for c := 0; c < len(grid); c = c + blockSize {
				val := map2[toArray2(getBlock(grid, r, c, blockSize))]
				setBlock(newGrid, toSlice3(&val), r/2*3, c/2*3)
			}
		}
		return newGrid
	}
	if blockSize == 3 {
		newGrid := buildGrid(len(grid) / 3 * 4)
		for r := 0; r < len(grid); r = r + blockSize {
			for c := 0; c < len(grid); c = c + blockSize {
				val := map3[toArray3(getBlock(grid, r, c, blockSize))]
				setBlock(newGrid, toSlice4(&val), r/3*4, c/3*4)
			}
		}
		return newGrid
	}
	return nil
}

func buildGrid(size int) [][]bool {
	grid := make([][]bool, size)
	for r := 0; r < size; r++ {
		grid[r] = make([]bool, size)
	}
	return grid
}

func getBlock(grid [][]bool, startR, startC, size int) [][]bool {
	block := make([][]bool, size)
	for r := 0; r < size; r++ {
		block[r] = grid[r+startR][startC : startC+size]
	}
	return block
}

func setBlock(grid [][]bool, block [][]bool, startR, startC int) {
	size := len(block)
	for r := 0; r < size; r++ {
		for c := 0; c < size; c++ {
			grid[r+startR][c+startC] = block[r][c]
		}
	}
}

func loadRulesFromFile(fileName string) (map[[2][2]bool][3][3]bool, map[[3][3]bool][4][4]bool) {
	dat, _ := ioutil.ReadFile(fileName)
	stringFile := strings.TrimSpace(string(dat))
	lines := strings.Split(stringFile, "\n")
	return loadRules(lines)
}

func loadRules(lines []string) (map[[2][2]bool][3][3]bool, map[[3][3]bool][4][4]bool) {
	map2 := make(map[[2][2]bool][3][3]bool)
	map3 := make(map[[3][3]bool][4][4]bool)
	for _, line := range lines {
		splitLine := strings.Split(line, " => ")
		inputS := strings.Split(splitLine[0], "/")
		outputS := strings.Split(splitLine[1], "/")
		switch len(inputS) {
		case 2:
			var output [3][3]bool
			slice := toSlice3(&output)
			parse(outputS, slice)

			var input [2][2]bool
			slice = toSlice2(&input)
			parse(inputS, slice)
			map2[input] = output
			rotate(slice)
			map2[input] = output
			rotate(slice)
			map2[input] = output
			rotate(slice)
			map2[input] = output

			slice = toSlice2(&input)
			flipAlongVertical(slice)
			map2[input] = output
			rotate(slice)
			map2[input] = output
			rotate(slice)
			map2[input] = output
			rotate(slice)
			map2[input] = output

			slice = toSlice2(&input)
			flipAlongHorizontal(slice)
			map2[input] = output
			rotate(slice)
			map2[input] = output
			rotate(slice)
			map2[input] = output
			rotate(slice)
			map2[input] = output

		case 3:
			var output [4][4]bool
			slice := toSlice4(&output)
			parse(outputS, slice)

			var input [3][3]bool
			slice = toSlice3(&input)
			parse(inputS, slice)
			map3[input] = output
			rotate(slice)
			map3[input] = output
			rotate(slice)
			map3[input] = output
			rotate(slice)
			map3[input] = output

			slice = toSlice3(&input)
			flipAlongVertical(slice)
			map3[input] = output
			rotate(slice)
			map3[input] = output
			rotate(slice)
			map3[input] = output
			rotate(slice)
			map3[input] = output

			slice = toSlice3(&input)
			flipAlongHorizontal(slice)
			map3[input] = output
			rotate(slice)
			map3[input] = output
			rotate(slice)
			map3[input] = output
			rotate(slice)
			map3[input] = output
		}

	}

	return map2, map3
}

func flipAlongVertical(s [][]bool) {
	for r := 0; r < len(s); r++ {
		for i, j := 0, len(s[r])-1; i < j; i, j = i+1, j-1 {
			s[r][i], s[r][j] = s[r][j], s[r][i]
		}
	}
}

func flipAlongHorizontal(s [][]bool) {
	for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
		for c := 0; c < len(s); c++ {
			s[i][c], s[j][c] = s[j][c], s[i][c]
		}
	}
}

func rotate(s [][]bool) {
	for ra, cb, rc, cd := 0, len(s)-1, len(s)-1, 0; ra < len(s); ra, cb, rc, cd = ra+1, cb-1, rc-1, cd+1 {
		for ca, rb, cc, rd := ra, ra, len(s)-1-ra, len(s)-1-ra; ca < len(s)-ra-1; ca, rb, cc, rd = ca+1, rb+1, cc-1, rd-1 {
			s[ra][ca], s[rb][cb], s[rc][cc], s[rd][cd] = s[rd][cd], s[ra][ca], s[rb][cb], s[rc][cc]
		}
	}
}

func toArray2(slice [][]bool) [2][2]bool {
	var array [2][2]bool
	for i := range array {
		for j := range array {
			array[i][j] = slice[i][j]
		}
	}
	return array
}

func toArray3(slice [][]bool) [3][3]bool {
	var array [3][3]bool
	for i := range array {
		for j := range array {
			array[i][j] = slice[i][j]
		}
	}
	return array
}

func toSlice2(array *[2][2]bool) [][]bool {
	slice := make([][]bool, len(array))
	for i, _ := range array {
		slice[i] = array[i][:]
	}
	return slice
}

func toSlice3(array *[3][3]bool) [][]bool {
	slice := make([][]bool, len(array))
	for i, _ := range array {
		slice[i] = array[i][:]
	}
	return slice
}

func toSlice4(array *[4][4]bool) [][]bool {
	slice := make([][]bool, len(array))
	for i, _ := range array {
		slice[i] = array[i][:]
	}
	return slice
}

func parse(inputS []string, output [][]bool) {
	for i, row := range output {
		for j, _ := range row {
			if rune(inputS[i][j]) == '.' {
				row[j] = false
			} else {
				row[j] = true
			}
		}
	}
}
