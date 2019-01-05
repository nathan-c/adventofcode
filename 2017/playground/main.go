package main

import (
	"fmt"
	"strconv"
)

func main() {

	var a, b [4][4]string
	for i := range b {
		for j := range b[i] {
			b[i][j] = strconv.Itoa(i + j)
		}
	}
	a = b
	print2D(a)
	print2D(b)
	rotate(toSlice4(&b))
	print2D(b)

	b = a
	flipAlongVertical(toSlice4(&b))
	print2D(b)
	b = a
	flipAlongHorizontal(toSlice4(&b))
	print2D(b)
}

func print2D(s [4][4]string) {
	for _, r := range s {
		fmt.Println(r)
	}
	fmt.Println()
}

func flipAlongVertical(s [][]string) {
	for r := 0; r < len(s); r++ {
		for i, j := 0, len(s[r])-1; i < j; i, j = i+1, j-1 {
			s[r][i], s[r][j] = s[r][j], s[r][i]
		}
	}
}

func flipAlongHorizontal(s [][]string) {
	for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
		for c := 0; c < len(s); c++ {
			s[i][c], s[j][c] = s[j][c], s[i][c]
		}
	}
}

func rotate(s [][]string) {
	for ra, cb, rc, cd := 0, len(s)-1, len(s)-1, 0; ra < len(s); ra, cb, rc, cd = ra+1, cb-1, rc-1, cd+1 {
		for ca, rb, cc, rd := ra, ra, len(s)-1-ra, len(s)-1-ra; ca < len(s)-ra-1; ca, rb, cc, rd = ca+1, rb+1, cc-1, rd-1 {
			s[ra][ca], s[rb][cb], s[rc][cc], s[rd][cd] = s[rd][cd], s[ra][ca], s[rb][cb], s[rc][cc]
		}
	}
}

func toSlice4(array *[4][4]string) [][]string {
	slice := make([][]string, len(array))
	for i, _ := range array {
		slice[i] = array[i][:]
	}
	return slice
}
