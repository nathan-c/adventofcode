package main

import (
	"fmt"
	"testing"
)

func Test_spin(t *testing.T) {
	tests := []struct {
		name   string
		input  []rune
		number int
		want   []rune
	}{
		{"test1", []rune{'a', 'b', 'c', 'd', 'e'}, 3, []rune{'c', 'd', 'e', 'a', 'b'}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			spin(&tt.input, tt.number)
			for i, actual := range tt.input {
				if actual != tt.want[i] {
					t.Errorf("spin(%v, %v)[%v] = %v, want %v", tt.input, tt.number, i, actual, tt.want[i])
				}
			}
		})
	}
}

func Test_exchange(t *testing.T) {
	tests := []struct {
		name  string
		input []rune
		a     int
		b     int
		want  []rune
	}{
		{"test1", []rune{'a', 'b', 'c', 'd', 'e'}, 2, 4, []rune{'a', 'b', 'e', 'd', 'c'}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			exchange(&tt.input, tt.a, tt.b)
			for i, actual := range tt.input {
				if actual != tt.want[i] {
					t.Errorf("exchange(%v, %v, %v)[%v] = %v, want %v", tt.input, tt.a, tt.b, i, actual, tt.want[i])
				}
			}
		})
	}
}

func Test_partner(t *testing.T) {
	tests := []struct {
		name  string
		input []rune
		a     rune
		b     rune
		want  []rune
	}{
		{"test1", []rune{'a', 'b', 'c', 'd', 'e'}, 'e', 'c', []rune{'a', 'b', 'e', 'd', 'c'}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			partner(&tt.input, tt.a, tt.b)
			for i, actual := range tt.input {
				if actual != tt.want[i] {
					t.Errorf("partner(%v, %v, %v)[%v] = %v, want %v", tt.input, tt.a, tt.b, i, actual, tt.want[i])
				}
			}
		})
	}
}

func Test_parseInput(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  []rune
	}{
		{"test1", "s1,x3/4,pe/b", []rune{'b', 'a', 'e', 'd', 'c'}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			programs := getPrograms(5)
			for instruction := range parseInput(tt.input) {
				instruction(programs)
			}
			if fmt.Sprint(programs) != fmt.Sprint(tt.want) {
				t.Errorf("parseInput(%v) = %v, want %v", tt.input, programs, tt.want)
			}
		})
	}
}

func Test_part2(t *testing.T) {
	tests := []struct {
		name       string
		input      string
		size       int
		iterations int
		want       string
	}{
		{"test1", "s1,x3/4,pe/b", 5, 2, "ceabd"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			output := part2(tt.input, tt.size, tt.iterations)
			if output != fmt.Sprint(tt.want) {
				t.Errorf("part2(%v, %v, %v) = %v, want %v", tt.input, tt.size, tt.iterations, output, tt.want)
			}
		})
	}
}
