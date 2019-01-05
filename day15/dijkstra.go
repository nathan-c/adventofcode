/*
Package dijkstra is an highly optimised implementation of the Dijkstra
algorithm, used for find the shortest path between points of a graph.

A graph is a map of points and map to the neighbouring points in the graph and
the cost to reach them.
A trivial example of a graph definition is:

	Graph{
		"a": {"b": 10, "c": 20},
		"b": {"a": 50},
		"c": {"b": 10, "a": 25},
	}

Borrowed and tweaked this implementation from https://github.com/albertorestifo/dijkstra

*/
package main

import (
	"fmt"
	"math"
	"strings"
)

type boolMatrix struct {
	data []bool
	w, h int
}

func newBoolMatrix(w, h int) boolMatrix {
	return boolMatrix{data: make([]bool, w*h), w: w, h: h}
}

func (m boolMatrix) get(x, y int) bool {
	return m.data[m.w*y+x]
}

func (m boolMatrix) set(x, y int, v bool) {
	m.data[m.w*y+x] = v
}

func (m boolMatrix) String() string {
	var sb strings.Builder
	for y := 0; y < m.h; y++ {
		for x := 0; x < m.w; x++ {
			if m.get(x, y) {
				sb.WriteRune('X')
			} else {
				sb.WriteRune('.')
			}
		}
		sb.WriteRune('\n')
	}
	return sb.String()
}

type locationSliceMatrix struct {
	data [][]location
	w, h int
}

func newLocationSliceMatrix(w, h int) locationSliceMatrix {
	return locationSliceMatrix{data: make([][]location, w*h), w: w, h: h}
}

func (m locationSliceMatrix) get(x, y int) []location {
	return m.data[m.w*y+x]
}

func (m locationSliceMatrix) set(x, y int, v []location) {
	m.data[m.w*y+x] = v
}

func getFirstStep(previous locationSliceMatrix, start, end location) location {
	firstStep := location{math.MaxInt64, math.MaxInt64}
	if parents := previous.get(end.x, end.y); parents != nil {
		for _, parent := range parents {
			if parent == start {
				if end.Less(firstStep) {
					firstStep = end
				}
			} else {
				step := getFirstStep(previous, start, parent)
				if step.Less(firstStep) {
					firstStep = step
				}
			}
		}
	}
	return firstStep
}

// Path finds the shortest paths between start and target.
// When multiple shortest paths have same cost they are all returned
func (u unitMap) shortestPath(start, target, max location) (firstStep location, pathLength int) {

	explored := newBoolMatrix(max.x, max.y)          // set of nodes we already explored
	frontier := newQueue()                           // queue of the nodes to explore
	previous := newLocationSliceMatrix(max.x, max.y) // previously visited nodes (with equal cost)

	// add starting point to the frontier as it'll be the first node visited
	frontier.Push(start, 0)

	// run until we visited every node in the frontier
	for frontier.Len() > 0 {
		// get the node in the frontier with the lowest cost (or priority)
		aKey, aPriority := frontier.Pop()

		// when the node with the lowest cost in the frontier is target, we can
		// compute the cost and path and exit the loop
		if aKey == target {
			pathLength = aPriority
			firstStep = getFirstStep(previous, start, aKey)
			return
		}

		// add the current node to the explored set
		explored.set(aKey.x, aKey.y, true)

		if aKey.x == 13 && aKey.y == 6 {
			fmt.Print()
		}

		// loop all the neighboring nodes
		for _, nKey := range u.openSquaresInRangeOf(aKey) {

			// skip already-explored nodes
			if explored.get(nKey.x, nKey.y) {
				continue
			}

			// if the node is not yet in the frontier add it with the cost
			if _, ok := frontier.Contains(nKey); !ok {
				locs := make([]location, 1, 4)
				locs[0] = aKey
				previous.set(nKey.x, nKey.y, locs)
				frontier.Push(nKey, aPriority+1)
				continue
			}

			frontierCost, _ := frontier.Contains(nKey)
			nodeCost := aPriority + 1

			// only update the cost of this node in the frontier when
			// it's below what's currently set or cost is equal
			if nodeCost < frontierCost {
				locs := make([]location, 1, 4)
				locs[0] = aKey
				previous.set(nKey.x, nKey.y, locs)
				frontier.Push(nKey, nodeCost)
			} else if nodeCost == frontierCost {
				previous.set(nKey.x, nKey.y, append(previous.get(nKey.x, nKey.y), aKey))
				frontier.Push(nKey, nodeCost)
			}
		}
	}

	return
}
