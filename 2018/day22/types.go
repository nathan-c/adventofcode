package main

type location struct {
	x, y int
}

func newCaves(depth int, target location) caves {
	return caves{depth, target, make(map[location]int)}
}

type equipment int

const (
	torch equipment = iota
	climbingGear
	nothing
)

type allowedEquipment []equipment

func (l allowedEquipment) contains(e equipment) bool {
	for _, x := range l {
		if x == e {
			return true
		}
	}
	return false
}

type region int

const (
	rocky  region = 0
	wet    region = 1
	narrow region = 2
)

var allowedTools = map[region]allowedEquipment{
	rocky:  {torch, climbingGear},
	wet:    {climbingGear, nothing},
	narrow: {torch, nothing},
}

var allowedRegions = map[equipment][]region{
	torch:        {rocky, narrow},
	climbingGear: {wet, rocky},
	nothing:      {wet, narrow},
}
