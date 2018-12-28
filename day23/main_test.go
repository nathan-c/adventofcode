package main

import "testing"

func Test_part1(t *testing.T) {
	tests := []struct {
		name string
		args []nanoBot
		want int
	}{
		{"example", readInput("test.txt"), 7},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := part1(tt.args); got != tt.want {
				t.Errorf("part1() = %v, want %v", got, tt.want)
			}
		})
	}
}
