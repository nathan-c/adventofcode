// This example demonstrates a priority queue built using the heap interface.
package main

import (
	"container/heap"
)

// An Item is something we manage in a priority queue.
type Item struct {
	value    location // The value of the item; arbitrary.
	priority int      // The priority of the item in the queue.
	// The index is needed by update and is maintained by the heap.Interface methods.
	index int // The index of the item in the heap.
}

// A PriorityQueueData implements heap.Interface and holds Items.
type PriorityQueueData struct {
	items []*Item
	nodes map[location]*Item
}

func (pq PriorityQueueData) Len() int { return len(pq.items) }

func (pq PriorityQueueData) Less(i, j int) bool {
	return pq.items[i].priority < pq.items[j].priority
}

func (pq PriorityQueueData) Swap(i, j int) {
	pq.items[i], pq.items[j] = pq.items[j], pq.items[i]
	pq.items[i].index = i
	pq.items[j].index = j
}

func (pq *PriorityQueueData) Push(x interface{}) {
	n := len(pq.items)
	item := x.(*Item)
	if _, ok := pq.nodes[item.value]; ok {
		return
	}
	item.index = n
	pq.items = append(pq.items, item)
	pq.nodes[item.value] = item
}

func (pq *PriorityQueueData) Pop() interface{} {
	old := *pq
	n := len(old.items)
	item := old.items[n-1]
	item.index = -1 // for safety
	pq.items = old.items[0 : n-1]
	delete(pq.nodes, item.value)
	return item
}

type PriorityQueue struct {
	d *PriorityQueueData
}

func (pq *PriorityQueue) Push(value location, priority int) {
	heap.Push(pq.d, &Item{value: value, priority: priority})
}

func (pq *PriorityQueue) Pop() (value location, priority int) {
	i := heap.Pop(pq.d).(*Item)
	return i.value, i.priority
}

func (pq *PriorityQueue) Len() int { return pq.d.Len() }

func (pq *PriorityQueue) Contains(value location) (int, bool) {
	if i, ok := pq.d.nodes[value]; ok {
		return i.priority, true
	}
	return -1, false
}

func newPriorityQueue() *PriorityQueue {
	d := PriorityQueueData{make([]*Item, 0, 10), make(map[location](*Item))}
	return &PriorityQueue{&d}
}
