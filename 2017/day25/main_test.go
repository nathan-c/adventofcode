package main

import (
	"testing"
)

func Test_runPart1(t *testing.T) {
	type args struct {
		blueprint  map[rune]step
		beginState rune
		steps      int
	}
	tests := []struct {
		name string
		args args
		want int
	}{
		{"test1", args{
			map[rune]step{
				'A': step{'A', [2]stepInternal{{1, 1, 'B'}, {0, -1, 'B'}}},
				'B': step{'B', [2]stepInternal{{1, -1, 'A'}, {1, 1, 'A'}}},
			}, 'A', 5}, 3},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := runPart1(tt.args.blueprint, tt.args.beginState, tt.args.steps); got != tt.want {
				t.Errorf("runPart1() = %v, want %v", got, tt.want)
			}
		})
	}
}
