package main

import (
	"reflect"
	"testing"
)

func Test_buildUsingTree(t *testing.T) {

	tests := []struct {
		name   string
		points []*point
		want   []*constellation
	}{
		{"simple 2 constellation test", []*point{{0, 0, 0, 0}, {1, 2, 3, 4}},
			[]*constellation{
				&constellation{[]*point{{0, 0, 0, 0}}},
				&constellation{[]*point{{1, 2, 3, 4}}},
			},
		},
		{"simple 1 constellation test", []*point{{0, 0, 0, 0}, {1, 0, 0, 0}},
			[]*constellation{
				&constellation{[]*point{{0, 0, 0, 0}, {1, 0, 0, 0}}},
			},
		},
		{"complex 2 constellation test", []*point{{0, 0, 0, 0}, {1, 0, 0, 0}, {1, 2, 3, 4}, {1, 2, 3, 6}},
			[]*constellation{
				&constellation{[]*point{{0, 0, 0, 0}, {1, 0, 0, 0}}},
				&constellation{[]*point{{1, 2, 3, 4}, {1, 2, 3, 6}}},
			},
		},
		{"complex 1 constellation test", []*point{
			{6, -2, -1, 0},
			{6, -3, 1, 0},
			{6, -3, 2, 0},
			{6, -2, 3, 0},
			{6, -3, -1, 2},
			{7, -2, 0, 2},
			{8, -2, 0, 1},
		},
			[]*constellation{
				&constellation{[]*point{
					{6, -3, 1, 0},
					{6, -3, 2, 0},
					{6, -2, 3, 0},
					{6, -2, -1, 0},
					{6, -3, -1, 2},
					{7, -2, 0, 2},
					{8, -2, 0, 1}}},
			},
		},
		{"complex 3 constellation test", []*point{
			{2, 6, 0, -6}, {3, 6, 1, -6}, {4, 6, 0, -7}, {4, 5, 0, -5}, {2, 6, 4, -4}, {3, 5, 2, -8},
		},
			[]*constellation{
				&constellation{[]*point{
					{2, 6, 0, -6}, {3, 6, 1, -6}, {4, 6, 0, -7}, {4, 5, 0, -5},
				}},
				&constellation{[]*point{{2, 6, 4, -4}}},
				&constellation{[]*point{{3, 5, 2, -8}}},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := buildUsingTree(tt.points); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("buildUsingTree() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_part1(t *testing.T) {
	tests := []struct {
		name     string
		testFile string
		want     int
	}{
		{"test1", "test1.txt", 4},
		{"test2", "test2.txt", 3},
		{"test3", "test3.txt", 8},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			points := parseInput(tt.testFile)
			if got := part1(points); got != tt.want {
				t.Errorf("part1() = %v, want %v", got, tt.want)
			}
		})
	}
}
