package main

import (
	"fmt"
	"sort"
	"testing"
)

func Test_part1(t *testing.T) {
	tests := []struct {
		name            string
		args            string
		wantLongestPath int
	}{
		{"short", "^WNE$", 3},
		{"medium", "^ENWWW(NEEE|SSE(EE|N))$", 10},
		{"medium2", "^ENWWW(SSE(EE|N)|NEEE)$", 10},
		{"long", "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$", 18},
		{"long2", "^ENNWSWW(|NEWS)SSSEEN(WNSE|)EE(SWEN|)NNN$", 18},
		{"very long", "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$", 23},
		{"very very long", "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$", 31},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotLongestPath, _ := part1([]rune(tt.args))
			if gotLongestPath != tt.wantLongestPath {
				t.Errorf("part1() gotLongestPath = %v, want %v", gotLongestPath, tt.wantLongestPath)
			}
		})
	}
}

func Test_part2(t *testing.T) {
	tests := []struct {
		name      string
		args      string
		x         int
		wantCount int
	}{
		{"short", "^WNE$", 3, 1},
		{"medium", "^ENWWW(NEEE|SSE(EE|N))$", 9, 4},
		{"medium2", "^ENWWW(SSE(EE|N)|NEEE)$", 9, 4},
		{"long", "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$", 15, 7},
		{"long2", "^ENNWSWW(|NEWS)SSSEEN(WNSE|)EE(SWEN|)NNN$", 15, 7},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			allRooms := make(map[location]*room)
			allRooms[location{0, 0}] = &room{location{0, 0}, nil, nil, 0}
			_, _ = buildGraph([]rune(tt.args), allRooms[location{0, 0}], allRooms)
			//printGraph(allRooms)
			count := countRoomsWithShortestPathGtX(allRooms, tt.x)
			if count != tt.wantCount {
				t.Errorf("got %v, want %v", count, tt.wantCount)
			}
		})
	}
}

type sortableLocations []location

func printGraph(allRooms map[location]*room) {
	keys := make([]location, 0, len(allRooms))
	for k := range allRooms {
		keys = append(keys, k)
	}
	sort.Slice(keys, func(i, j int) bool {
		a, b := keys[i], keys[j]
		if a.y == b.y {
			return a.x < b.x
		}
		return a.y < b.y
	})

	for _, k := range keys {
		v := allRooms[k]
		locs := make([]string, len(v.doors), len(v.doors))
		for i, d := range v.doors {
			locs[i] = fmt.Sprintf("(%v %v)", d.x, d.y)
		}
		fmt.Printf("(%v %v,: %v)\t->\t%v\n", k.x, k.y, v.distance, locs)
	}
}

func Test_hasCycles(t *testing.T) {
	tests := []struct {
		name string
		args string
	}{
		{"short", "^WNE$"},
		{"medium", "^ENWWW(NEEE|SSE(EE|N))$"},
		{"medium2", "^ENWWW(SSE(EE|N)|NEEE)$"},
		{"long", "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"},
		{"long2", "^ENNWSWW(|NEWS)SSSEEN(WNSE|)EE(SWEN|)NNN$"},
		{"very long", "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"},
		{"very very long", "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			hasCycles := checkCycles([]rune(tt.args), location{0, 0}, make(map[location]bool))
			if hasCycles {
				t.Errorf("checkCycles(%v) has cycles", tt.args)
			}
		})
	}
}
