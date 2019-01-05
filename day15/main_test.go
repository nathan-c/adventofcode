package main

import (
	"testing"
)

// func Test_location_Less(t *testing.T) {
// 	type args struct {
// 		this  location
// 		other location
// 	}
// 	tests := []struct {
// 		name string
// 		args args
// 		want bool
// 	}{
// 		{"lt1", args{location{1, 1}, location{1, 2}}, true},
// 		{"lt2", args{location{1, 2}, location{2, 2}}, true},
// 		{"lt3", args{location{2, 1}, location{3, 2}}, true},
// 		{"lt4", args{location{2, 1}, location{1, 2}}, true},
// 		{"lt5", args{location{2, 1}, location{2, 3}}, true},
// 		{"gt1", args{location{1, 2}, location{2, 1}}, false},
// 		{"gt2", args{location{2, 2}, location{1, 2}}, false},
// 		{"gt3", args{location{2, 3}, location{2, 1}}, false},
// 		{"gt4", args{location{2, 3}, location{1, 2}}, false},
// 		{"gt5", args{location{2, 3}, location{3, 2}}, false},
// 		{"eq", args{location{2, 3}, location{2, 3}}, false},
// 	}
// 	for _, tt := range tests {
// 		t.Run(tt.name, func(t *testing.T) {
// 			if got := tt.args.this.Less(tt.args.other); got != tt.want {
// 				t.Errorf("location.Less() = %v, want %v", got, tt.want)
// 			}
// 		})
// 	}
// }

func Test_part1(t *testing.T) {
	tests := []struct {
		name        string
		args        string
		wantOutcome int
	}{
		{"test1", "test1.txt", 27730},
		{"test2", "test2.txt", 36334},
		{"test3", "test3.txt", 39514},
		{"test4", "test4.txt", 27755},
		{"test5", "test5.txt", 28944},
		{"test6", "test6.txt", 18740},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			u, l := parseInput(tt.args, 3)
			if gotOutcome := part1(u, l); gotOutcome != tt.wantOutcome {
				t.Errorf("part1() = %v, want %v", gotOutcome, tt.wantOutcome)
			}
		})
	}
}

func Test_part2(t *testing.T) {
	tests := []struct {
		fileName    string
		wantOutcome int
	}{
		{"input.txt", 41972},
		{"test1.txt", 4988},
		{"test3.txt", 31284},
		{"test4.txt", 3478},
		{"test5.txt", 6474},
		{"test6.txt", 1140},
	}
	for _, tt := range tests {
		t.Run(tt.fileName, func(t *testing.T) {
			if gotOutcome := part2(tt.fileName, 4); gotOutcome != tt.wantOutcome {
				t.Errorf("part2() = %v, want %v", gotOutcome, tt.wantOutcome)
			}
		})
	}
}

type test struct {
	i    int
	in   input
	oout output
}

func Benchmark_runTurn(b *testing.B) {
	m, l := parseInput("input.txt", 3)
	for n := 0; n < b.N; n++ {
		runTurn(m, l)
	}
}
