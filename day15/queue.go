// Borrowed this implementation from https://github.com/albertorestifo/dijkstra
package main

import "sort"

// queue is a basic priority queue implementation, where the node with the
// lowest priority is kept as first element in the queue
type queue struct {
	// keys  []location
	nodeSet map[location]struct{}
	nodes   []node
}

type node struct {
	l location
	p int
}

// Len is part of sort.Interface
func (q *queue) Len() int {
	return len(q.nodes)
}

// Swap is part of sort.Interface
func (q *queue) Swap(i, j int) {
	q.nodes[i], q.nodes[j] = q.nodes[j], q.nodes[i]
}

// Less is part of sort.Interface
func (q *queue) Less(i, j int) bool {
	a := q.nodes[i]
	b := q.nodes[j]

	return a.p < b.p
}

// set updates or inserts a new key in the priority queue
func (q *queue) Push(key location, priority int) {
	// inserts a new key if we don't have it already
	if _, ok := q.nodeSet[key]; !ok {
		q.nodes = append(q.nodes, node{key, priority})
		q.nodeSet[key] = struct{}{}
	}

	// sort the keys array
	sort.Sort(q)
}

// Next removes the first element from the queue and retuns it's key and priority
func (q *queue) Pop() (key location, priority int) {
	// shift the key form the queue
	node, nodes := q.nodes[0], q.nodes[1:]
	q.nodes = nodes
	key = node.l
	priority = node.p

	delete(q.nodeSet, key)

	return key, priority
}

// isEmpty returns true when the queue is empty
func (q *queue) isEmpty() bool {
	return len(q.nodes) == 0
}

// get returns the priority of a passed key
func (q *queue) Contains(key location) (priority int, ok bool) {
	if _, ok = q.nodeSet[key]; ok {
		for _, n := range q.nodes {
			if n.l == key {
				priority = n.p
				return
			}
		}
	}
	return
}

// NewQueue creates a new empty priority queue
func newQueue() *queue {
	var q queue
	q.nodes = make([]node, 0, 10)
	q.nodeSet = make(map[location]struct{})
	return &q
}
