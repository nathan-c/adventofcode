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
)

type node struct {
	key  location
	cost int
}

type linkedList struct {
	head   *linkedListNode
	tail   *linkedListNode
	length int
}

type linkedListNode struct {
	value location
	next  *linkedListNode
}

func (l *linkedList) clone() *linkedList {
	return &linkedList{l.head, l.tail, l.length}
}

func (l *linkedListNode) pushHead(value location) *linkedListNode {
	return &linkedListNode{
		value: value,
		next:  l,
	}
}

func getFirstStep(previous map[location][]*location, start, end *location) *location {
	var firstStep *location
	if parents, ok := previous[*end]; ok {
		for _, parent := range parents {
			if *parent == *start {
				if firstStep == nil || end.Less(*firstStep) {
					firstStep = end
				}
			} else {
				step := getFirstStep(previous, start, parent)
				if firstStep == nil || step.Less(*firstStep) {
					firstStep = step
				}
			}
		}
	}
	return firstStep
}

// Path finds the shortest paths between start and target.
// When multiple shortest paths have same cost they are all returned
func (u unitMap) shortestPath(start, target location) (firstStep *location, pathLength int) {

	explored := make(map[location]bool)        // set of nodes we already explored
	frontier := newQueue()                     // queue of the nodes to explore
	previous := make(map[location][]*location) // previously visited nodes (with equal cost)

	// add starting point to the frontier as it'll be the first node visited
	frontier.Push(start, 0)

	// run until we visited every node in the frontier
	for frontier.Len() > 0 {
		// get the node in the frontier with the lowest cost (or priority)
		aKey, aPriority := frontier.Pop()
		n := node{aKey, aPriority}

		// when the node with the lowest cost in the frontier is target, we can
		// compute the cost and path and exit the loop
		if n.key == target {
			pathLength = n.cost
			firstStep = getFirstStep(previous, &start, &n.key)
			return
		}

		// add the current node to the explored set
		explored[n.key] = true

		if n.key.x == 13 && n.key.y == 6 {
			fmt.Print()
		}

		// loop all the neighboring nodes
		for _, nKey := range u.openSquaresInRangeOf(n.key) {

			// skip already-explored nodes
			if explored[nKey] {
				continue
			}

			// if the node is not yet in the frontier add it with the cost
			if _, ok := frontier.Contains(nKey); !ok {
				previous[nKey] = []*location{&n.key}
				frontier.Push(nKey, n.cost+1)
				continue
			}

			frontierCost, _ := frontier.Contains(nKey)
			nodeCost := n.cost + 1

			// only update the cost of this node in the frontier when
			// it's below what's currently set or cost is equal
			if nodeCost < frontierCost {
				previous[nKey] = []*location{&n.key}
				frontier.Push(nKey, nodeCost)
			} else if nodeCost == frontierCost {
				previous[nKey] = append(previous[nKey], &n.key)
				frontier.Push(nKey, nodeCost)
			}
		}
	}

	return
}
