// Borrowed this implementation from https://github.com/albertorestifo/dijkstra
package main

import "sort"

// queue is a basic priority queue implementation, where the node with the
// lowest priority is kept as first element in the queue
type queue struct {
	keys  []locationAndEquipment
	nodes map[locationAndEquipment]int
}

// Len is part of sort.Interface
func (q *queue) Len() int {
	return len(q.keys)
}

// Swap is part of sort.Interface
func (q *queue) Swap(i, j int) {
	q.keys[i], q.keys[j] = q.keys[j], q.keys[i]
}

// Less is part of sort.Interface
func (q *queue) Less(i, j int) bool {
	a := q.keys[i]
	b := q.keys[j]

	return q.nodes[a] < q.nodes[b]
}

// set updates or inserts a new key in the priority queue
func (q *queue) set(key locationAndEquipment, priority int) {
	// inserts a new key if we don't have it already
	if _, ok := q.nodes[key]; !ok {
		q.keys = append(q.keys, key)
	}

	// set the priority for the key
	q.nodes[key] = priority

	// sort the keys array
	sort.Sort(q)
}

// Next removes the first element from the queue and retuns it's key and priority
func (q *queue) next() (key locationAndEquipment, priority int) {
	// shift the key form the queue
	key, keys := q.keys[0], q.keys[1:]
	q.keys = keys

	priority = q.nodes[key]

	delete(q.nodes, key)

	return key, priority
}

// isEmpty returns true when the queue is empty
func (q *queue) isEmpty() bool {
	return len(q.keys) == 0
}

// get returns the priority of a passed key
func (q *queue) get(key locationAndEquipment) (priority int, ok bool) {
	priority, ok = q.nodes[key]
	return
}

// NewQueue creates a new empty priority queue
func newQueue() *queue {
	var q queue
	q.nodes = make(map[locationAndEquipment]int)
	return &q
}
