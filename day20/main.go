package main

import (
	"fmt"
	"io/ioutil"
	"log"
)

func main() {
	f, err := ioutil.ReadFile("input.txt")
	if err != nil {
		log.Panicf("could not open file. %v", err)
	}
	inputText := []rune(string(f))

	longestPath, _ := part1(inputText)
	fmt.Printf("part 1 longest path: %v\n", longestPath)
	allRooms := make(map[location]*room)
	_, _ = buildGraph(inputText, &room{location{0, 0}, nil, nil, 0}, allRooms)
	count := countRoomsWithShortestPathGtX(allRooms, 1000)
	fmt.Printf("part 2 graph: %v\n", count)
}

type location struct {
	x, y int
}

type room struct {
	location
	parent   *room
	doors    []*room
	distance int
}

func (r *room) addDoor(d *room) {
	if d.parent == nil {
		d.parent = r
	}
	r.doors = append(r.doors, d)
}

func part1(regex []rune) (longestPath int, endIndex int) {
	for i := 0; i < len(regex); i++ {
		r := regex[i]
		if r == '(' {
			l, e := part1(regex[i+1:])
			longestPath += l
			i += e + 1
		} else if r == ')' {
			return longestPath, i
		} else if r == '|' {
			l, e := part1(regex[i+1:])
			if l == 0 || longestPath > 0 && l > longestPath {
				return l, i + e + 1
			}
			return longestPath, i + e + 1
		} else if r != '$' && r != '^' {
			longestPath++
		}
	}
	return longestPath, len(regex)
}

func buildGraph(regex []rune, head *room, allRooms map[location]*room) (*room, int) {
	current := head
	for i := 0; i < len(regex); i++ {
		r := regex[i]
		if r == '(' {
			l, e := buildGraph(regex[i+1:], current, allRooms)
			current = l
			i += e + 1
		} else if r == ')' {
			return current, i
		} else if r == '|' {
			current = head
		} else if r != '$' && r != '^' {
			var newRoom *room
			var ok bool
			newLocation := increment(r, current.location)
			if newRoom, ok = allRooms[newLocation]; ok {
				if current.distance+1 < newRoom.distance {
					newRoom.distance = current.distance + 1
				}
			} else {
				newRoom = &room{newLocation, current, nil, current.distance + 1}
				allRooms[newLocation] = newRoom
			}
			current.addDoor(newRoom)
			current = newRoom
		}
	}
	return head, len(regex)
}

type distance struct {
	room *room
	i    int
}

func countRoomsWithShortestPathGtX(graph map[location]*room, x int) int {
	count := 0
	for _, v := range graph {
		if v.distance >= x {
			count++
		}
	}
	return count
}

func increment(direction rune, l location) location {
	switch direction {
	case 'N':
		return location{l.x, l.y + 1}
	case 'S':
		return location{l.x, l.y - 1}
	case 'E':
		return location{l.x + 1, l.y}
	case 'W':
		return location{l.x - 1, l.y}
	default:
		panic(fmt.Sprintf("cannot process direction %v", direction))
	}
}
