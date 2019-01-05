package main

import (
	"testing"
)

func Test_runPart1(t *testing.T) {
	type args struct {
		particles  []*particle
		iterations int
	}
	tests := []struct {
		name string
		args args
		want int
	}{
		{"test1",
			args{
				[]*particle{
					{[3]int{3, 0, 0}, [3]int{2, 0, 0}, [3]int{-1, 0, 0}, 0},
					{[3]int{4, 0, 0}, [3]int{0, 0, 0}, [3]int{-2, 0, 0}, 1},
				},
				5},
			0},
		{"test2",
			args{
				[]*particle{
					{[3]int{0, 3, 0}, [3]int{0, 2, 0}, [3]int{0, -1, 0}, 0},
					{[3]int{4, 0, 0}, [3]int{0, 0, 0}, [3]int{-2, 0, 0}, 1},
				},
				5},
			0},
		{"test3",
			args{
				[]*particle{
					{[3]int{0, 0, 3}, [3]int{0, 0, 2}, [3]int{0, 0, -1}, 0},
					{[3]int{4, 0, 0}, [3]int{0, 0, 0}, [3]int{-2, 0, 0}, 1},
				},
				100},
			0},
		{"test4",
			args{
				[]*particle{
					{[3]int{1609, -863, -779}, [3]int{-15, 54, -69}, [3]int{-10, 0, 14}, 0},
					{[3]int{-391, 1353, -387}, [3]int{-94, -42, 0}, [3]int{14, -5, 3}, 1},
					{[3]int{3329, -143, 333}, [3]int{-29, 9, -45}, [3]int{-21, 0, 3}, 2},
					{[3]int{1751, 3779, 3413}, [3]int{-8, -57, -80}, [3]int{-5, -8, -5}, 3},
					{[3]int{-3241, 1019, 17}, [3]int{75, 8, -26}, [3]int{5, -4, 2}, 4},
				},
				100000},
			4},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := runPart1(tt.args.particles, tt.args.iterations); got != tt.want {
				t.Errorf("run() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_runPart2(t *testing.T) {
	type args struct {
		particles  []*particle
		iterations int
	}
	tests := []struct {
		name string
		args args
		want int
	}{
		{"test1",
			args{
				[]*particle{
					{[3]int{-1, 0, 0}, [3]int{1, 0, 0}, [3]int{0, 0, 0}, 0},
					{[3]int{1, 0, 0}, [3]int{-1, 0, 0}, [3]int{0, 0, 0}, 1},
				},
				1},
			0},
		{"test2",
			args{
				[]*particle{
					{[3]int{-1, 0, 0}, [3]int{0, 0, 0}, [3]int{1, 0, 0}, 0},
					{[3]int{1, 0, 0}, [3]int{0, 0, 0}, [3]int{-1, 0, 0}, 1},
				},
				1},
			0},
		{"test3",
			args{
				[]*particle{
					{[3]int{-2, 0, 0}, [3]int{-2, 0, 0}, [3]int{1, 0, 0}, 0},
					{[3]int{2, 0, 0}, [3]int{2, 0, 0}, [3]int{-1, 0, 0}, 1},
				},
				10},
			0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := runPart2(tt.args.particles, tt.args.iterations); got != tt.want {
				t.Errorf("runPart2() = %v, want %v", got, tt.want)
			}
		})
	}
}
