package main

import (
	"reflect"
	"testing"
)

func Test_loadRules(t *testing.T) {
	type args struct {
		lines []string
	}
	tests := []struct {
		name     string
		args     args
		map2Size int
		map3Size int
	}{
		{"test1", args{[]string{"../.. => .#./.../###"}}, 1, 0},
		{"test2", args{[]string{"#./.. => .#./.../###"}}, 4, 0},
		{"test3", args{[]string{"##./#../... => ...#/..#./.###/#.#."}}, 0, 4},
		{"test3", args{[]string{"#./.. => .#./.../###", "##./#../... => ...#/..#./.###/#.#."}}, 4, 4},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, got1 := loadRules(tt.args.lines)
			if !reflect.DeepEqual(len(got), tt.map2Size) {
				t.Errorf("loadRules() got = %v, want %v", len(got), tt.map2Size)
			}
			if !reflect.DeepEqual(len(got1), tt.map3Size) {
				t.Errorf("loadRules() got1 = %v, want %v", len(got1), tt.map3Size)
			}
		})
	}
}

func Test_runIteration(t *testing.T) {
	type args struct {
		grid  [][]bool
		lines []string
	}
	tests := []struct {
		name string
		args args
		want int
	}{
		{
			"test1",
			args{
				[][]bool{{false, true, false}, {false, false, true}, {true, true, true}},
				[]string{"../.# => ##./#../...", ".#./..#/### => #..#/..../..../#..#"},
			},
			12,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			map2, map3 := loadRules(tt.args.lines)
			grid := tt.args.grid
			grid = runIteration(grid, map2, map3)
			grid = runIteration(grid, map2, map3)
			if !reflect.DeepEqual(countOn(grid), tt.want) {
				t.Errorf("runIteration() = %v, want %v", countOn(grid), tt.want)
			}
		})
	}
}
