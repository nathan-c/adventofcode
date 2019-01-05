package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
	f, _ := os.Open("input.txt")
	defer f.Close()
	g := parseInput(f)
	pour(g, g.spring.x, g.spring.y)
	fmt.Printf("total water: %v\n", g.countWater())
	ioutil.WriteFile("out.txt", []byte(g.String()), os.ModePerm)
}

func pour(g grid, x, starty int) {
	//fmt.Println(g)
	bottom := g.bottom()
	if starty == bottom {
		return
	}
	y := starty + 1
	below := g.get(x, y)
	switch below {
	case empty:
		g.set(x, y, runningWater)
		pour(g, x, y)
		pour(g, x, starty)
	case clay, standingWater:
		// search left for
		// #
		// #####
		// OR
		// search left for
		// #
		// #~~~~
		lx := x - 1
		for g.isClayOrStanding(lx, y) && g.get(lx, y-1) != clay {
			lx--
		}

		// search right for
		//     #
		// #####
		// OR
		// search right for
		//     #
		// ~~~~#
		rx := x + 1
		for g.isClayOrStanding(rx, y) && g.get(rx, y-1) != clay {
			rx++
		}

		if g.get(rx, y-1) == clay && g.get(lx, y-1) == clay {
			// if we now have
			// #    #
			// #~~~~#
			// then fill with standing water
			for x2 := lx + 1; x2 < rx; x2++ {
				g.set(x2, y-1, standingWater)
			}
			return
		} else {
			// else we now have
			// #   |
			// #~~~~#
			// then fill with standing water
			if g.get(x-1, y-1) == empty {
				pour(g, x-1, y-2)
			}
			if g.get(x+1, y-1) == empty {
				pour(g, x+1, y-2)
			}
		}
	}

}

func _pour(g grid, startx, starty int) {
	x := startx
	bottom := g.bottom()
	for y := starty + 1; y < bottom+1; y++ {
		fmt.Println(g)

		below := g.get(startx, y)
		switch below {
		case empty:
			g.set(x, y, runningWater)
		case clay, standingWater:
			// search left for
			// #
			// #####
			// OR
			// search left for
			// #
			// #~~~~
			lx := x - 1
			for g.isClayOrStanding(lx, y) && g.get(lx, y-1) != clay {
				lx--
			}

			// search right for
			//     #
			// #####
			// OR
			// search right for
			//     #
			// ~~~~#
			rx := x + 1
			for g.isClayOrStanding(rx, y) && g.get(rx, y-1) != clay {
				rx++
			}

			if g.get(rx, y-1) == clay && g.get(lx, y-1) == clay {
				// if we now have
				// #    #
				// #~~~~#
				// then fill with standing water
				for x2 := lx + 1; x2 < rx; x2++ {
					g.set(x2, y-1, standingWater)
				}
				y = y - 2
			} else {
				// else we now have
				// #   |
				// #~~~~#
				// then fill with standing water
				if g.get(x-1, y-1) == empty {
					pour(g, x-1, y-2)
				}
				if g.get(x+1, y-1) == empty {
					pour(g, x+1, y-2)
				}
				return
			}

		}
	}

}
