package main

type location struct {
	x, y int
}

func newLocation(x, y int) location {
	return location{x, y}
}

// l is less than other
func (l location) Less(other location) bool {
	if l.y < other.y {
		return true
	} else if l.y == other.y {
		if l.x < other.x {
			return true
		}
	}
	return false
}

func (l location) isInRange(other location) bool {
	dist := dist(l, other)
	if dist == 1 {
		return true
	}
	return false
}

func dist(a, b location) int {
	return abs(a.x-b.x) + abs(a.y-b.y)
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}
