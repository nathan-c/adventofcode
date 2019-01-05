package main

import (
	"testing"
)

func Test_part1(t *testing.T) {
	tests := []struct {
		name string
		args caves
		want int
	}{
		{"example", newCaves(510, location{10, 10}), 114},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := part1(tt.args); got != tt.want {
				t.Errorf("part1() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_part2(t *testing.T) {
	tests := []struct {
		name string
		args caves
		want int
	}{
		{"example", newCaves(510, location{10, 10}), 45},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := part2(tt.args); got != tt.want {
				t.Errorf("part2() = %v, want %v", got, tt.want)
			}
		})
	}
}
