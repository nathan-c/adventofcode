package main

import (
	"math"
	"testing"
)

func Test_generator(t *testing.T) {
	type args struct {
		previousNumber int
		factor         int
		dividor        int
	}
	tests := []struct {
		name string
		args args
		want []int
	}{
		{name: "test1", args: args{65, 16807, 2147483647}, want: []int{1092455, 1181022009, 245556042, 1744312007, 1352636452}},
		{name: "test2", args: args{8921, 16807, 2147483647}, want: []int{430625591, 1233683848, 1431495498, 137874439, 285222916}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			ch := generator(tt.args.previousNumber, tt.args.factor, tt.args.dividor)
			i := 0
			for actual := range ch {
				if actual != tt.want[i] {
					t.Errorf("generator(%v, %v, %v)[%v] = %v, want %v", tt.args.previousNumber, tt.args.factor, tt.args.dividor, i, actual, tt.want[i])
				}
				i++
				if i >= len(tt.want) {
					break
				}
			}
		})
	}
}

func Test_compareIntegers(t *testing.T) {
	tests := []struct {
		name string
		arg1 int
		arg2 int
		want bool
	}{
		{"test0", int(math.Pow(2, 16)), 0, true},
		{"test1", 1092455, 430625591, false},
		{"test2", 1181022009, 1233683848, false},
		{"test2", 245556042, 1431495498, true},
		{"test2", 1744312007, 137874439, false},
		{"test2", 1352636452, 285222916, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if actual := compareIntegers(tt.arg1, tt.arg2); actual != tt.want {
				t.Errorf("compareIntegers(%v, %v) = %v, want %v", tt.arg1, tt.arg2, actual, tt.want)
			}
		})
	}
}
