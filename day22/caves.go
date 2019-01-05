package main

type caves struct {
	depth             int
	target            location
	geologicIndexMemo map[location]int
}

func (c *caves) geologicIndex(p location) int {
	if p.x == 0 && p.y == 0 {
		return 0
	}
	if p == c.target {
		return 0
	}
	if p.y == 0 {
		return p.x * 16807
	}
	if p.x == 0 {
		return p.y * 48271
	}
	if gi, ok := c.geologicIndexMemo[p]; ok {
		return gi
	}
	gi := c.erosionLevel(location{p.x - 1, p.y}) * c.erosionLevel(location{p.x, p.y - 1})
	c.geologicIndexMemo[p] = gi
	return gi
}

func (c *caves) erosionLevel(p location) int {
	gi := c.geologicIndex(p)
	return (gi + c.depth) % 20183
}

func (c *caves) regionType(p location) region {
	el := c.erosionLevel(p)
	return region(el % 3)
}

func (c *caves) contains(l location) bool {
	if l.x < 0 {
		return false
	}
	if l.y < 0 {
		return false
	}
	if l.x > c.target.x+100 {
		return false
	}
	if l.y > c.target.y+100 {
		return false
	}
	return true
}

func (c *caves) getEdges(l locationAndEquipment) map[locationAndEquipment]int {
	top := location{l.x, l.y - 1}
	bottom := location{l.x, l.y + 1}
	left := location{l.x - 1, l.y}
	right := location{l.x + 1, l.y}

	edges := make(map[locationAndEquipment]int)

	r := c.regionType(l.location)
	tools := allowedTools[r]
	if tools[0] == l.equipment {
		edges[locationAndEquipment{l.location, tools[1]}] = 7
	} else {
		edges[locationAndEquipment{l.location, tools[0]}] = 7
	}

	addEdge := func(_l location) {
		if c.contains(_l) {
			r := c.regionType(_l)
			allowedT := allowedTools[r]
			if allowedT.contains(l.equipment) {
				edges[locationAndEquipment{_l, l.equipment}] = 1
			}
		}
	}
	addEdge(top)
	addEdge(bottom)
	addEdge(left)
	addEdge(right)
	return edges
}

func (c *caves) switchGear(from locationAndEquipment, to location) equipment {
	fromR := c.regionType(from.location)
	toR := c.regionType(to)

	if fromR == toR {
		return from.equipment
	}

	fromAllowed := allowedTools[fromR]
	toAllowed := allowedTools[toR]
	t := getTool(fromAllowed, toAllowed)
	return t
}

func getTool(as, bs allowedEquipment) equipment {
	for _, a := range as {
		for _, b := range bs {
			if a == b {
				return a
			}
		}
	}
	panic("no matching equipment")
}
