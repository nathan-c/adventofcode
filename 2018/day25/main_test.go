package main

import (
	"testing"
)

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
