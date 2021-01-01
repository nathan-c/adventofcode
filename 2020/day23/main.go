package main

import (
	"fmt"
)

var testInput string = "389125467"
var input string = "326519478"

func main() {
	input := readInput()
	partOneFast(input)
	partTwo(input)
}

func partTwo(input []int) {
	max := input[0]

	allNodes := make(map[int]*fastCircleNode)
	start := &fastCircleNode{label: input[0], allNodes: allNodes}
	allNodes[start.label] = start
	current := start
	for _, cup := range input[1:] {
		if cup > max {
			max = cup
		}
		node := &fastCircleNode{label: cup, allNodes: allNodes}
		allNodes[node.label] = node
		current.next = node
		current = node
	}

	for i := max + 1; i <= 1_000_000; i++ {
		node := &fastCircleNode{label: i, allNodes: allNodes}
		allNodes[node.label] = node
		current.next = node
		current = node
	}

	current.next = start
	current = start
	for i := 0; i < 10_000_000; i++ {
		current = moveFast(current)
	}

	cupOne := allNodes[1]
	fmt.Println(cupOne.next.label, cupOne.next.next.label, len(allNodes))
	fmt.Printf("Part 2: %v", cupOne.next.label*cupOne.next.next.label)
}

func partOne(input []int) {
	c := circle{cups: input, currentIdx: 0}
	fmt.Println(input)
	for i := 0; i < 100; i++ {
		c = move(c)
		fmt.Printf("%v, %v\n", c.cups, c.cups[c.currentIdx])
	}
	c.printResult()
}

func partOneFast(input []int) {

	allNodes := make(map[int]*fastCircleNode)
	start := &fastCircleNode{label: input[0], allNodes: allNodes}
	allNodes[start.label] = start
	current := start
	for _, cup := range input[1:] {
		node := &fastCircleNode{label: cup, allNodes: allNodes}
		allNodes[node.label] = node
		current.next = node
		current = node
	}

	current.next = start
	current = start
	current.printState()
	for i := 0; i < 100; i++ {
		current = moveFast(current)
		//current.printState()
	}

	for current.label != 1 {
		current = current.next
	}

	current = current.next

	for current.label != 1 {
		fmt.Print(current.label)
		current = current.next
	}
	fmt.Println()
}

func move(c circle) circle {
	p1, removed := c.removeThree()
	destination := p1.selectDest()
	p2 := p1.insert(destination, removed)
	p2.incrementIndex()
	return p2
}

func moveFast(c *fastCircleNode) *fastCircleNode {
	removed := c.removeThree()
	destination := c.selectDest()
	destination.insert(removed)

	return c.next
}

func readInput() []int {
	dataStr := input
	cups := make([]int, 0, len(dataStr))
	for _, x := range dataStr {
		cups = append(cups, int(x)-'0')
	}
	return cups
}

type circle struct {
	cups       []int
	currentIdx int
}

func (c *circle) removeThree() (circle, []int) {
	if c.currentIdx+3 >= len(c.cups) {
		start := (c.currentIdx + 4) % len(c.cups)
		return circle{cups: c.cups[start : c.currentIdx+1], currentIdx: c.currentIdx - start}, append(copyCups(c.cups[c.currentIdx+1:]), c.cups[:start]...)
	}
	return circle{cups: append(copyCups(c.cups[:c.currentIdx+1]), c.cups[c.currentIdx+4:]...), currentIdx: c.currentIdx}, c.cups[c.currentIdx+1 : c.currentIdx+4]
}

func (c *circle) insert(i int, cups []int) circle {
	currentIdx := 0
	if c.currentIdx < i {
		currentIdx = c.currentIdx
	} else {
		currentIdx = c.currentIdx + len(cups)
	}
	i = (i + 1) % len(c.cups)
	if i == 0 {
		return circle{cups: append(copyCups(c.cups), cups...), currentIdx: currentIdx}
	}
	return circle{cups: append(append(copyCups(c.cups[:i]), cups...), c.cups[i:]...), currentIdx: currentIdx}
}

func (c *circle) selectDest() int {
	currentCup := c.cups[c.currentIdx]
	min := c.minLabel()
	max := c.maxLabel()

	for value := currentCup - 1; ; value-- {
		if value < min {
			value = max
		}
		for i, cup := range c.cups {
			if value == cup {
				return i
			}
		}
	}
}

func (c *circle) minLabel() int {
	x := c.cups[0]
	for _, cup := range c.cups[1:] {
		if x > cup {
			x = cup
		}
	}
	return x
}

func (c *circle) maxLabel() int {
	x := c.cups[0]
	for _, cup := range c.cups[1:] {
		if x < cup {
			x = cup
		}
	}
	return x
}

func (c *circle) incrementIndex() {
	idx := c.currentIdx + 1
	c.currentIdx = idx % len(c.cups)
}

func copyCups(cups []int) []int {
	new := make([]int, len(cups))
	copy(new, cups)
	return new
}

func (c *circle) printResult() {
	idx := 0
	for i, x := range c.cups {
		if x == 1 {
			idx = i
			break
		}
	}
	for i := 1; i < len(c.cups); i++ {
		fmt.Print(c.cups[(idx+i)%len(c.cups)])
	}
	fmt.Println()
}

type fastCircleNode struct {
	label     int
	next      *fastCircleNode
	allNodes  map[int]*fastCircleNode
	isRemoved bool
}

func (c *fastCircleNode) removeThree() *fastCircleNode {

	start := c.next
	current := c.next
	for i := 0; i < 2; i++ {
		current.isRemoved = true
		current = current.next
	}
	current.isRemoved = true
	end := current

	c.next = end.next
	//start.prev = nil
	end.next = nil

	return start
}

func (c *fastCircleNode) selectDest() *fastCircleNode {
	currentCup := c.label
	possibleDestination := currentCup - 1

	for {
		if possibleDestination == 0 {
			possibleDestination = len(c.allNodes)
		}
		node := c.allNodes[possibleDestination]
		if !node.isRemoved {
			return node
		}
		possibleDestination--
	}
}

func (c *fastCircleNode) insert(cups *fastCircleNode) {
	current := cups
	for current.next != nil {
		current.isRemoved = false
		current = current.next
	}
	current.isRemoved = false
	next := c.next
	current.next = next
	c.next = cups
}

func (c *fastCircleNode) printState() {
	current := c
	fmt.Print(current.label)
	current = current.next
	for current != c {
		fmt.Print(current.label)
		current = current.next
	}
	fmt.Println()
}
