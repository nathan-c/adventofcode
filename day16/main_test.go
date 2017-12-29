package main

import (
	"fmt"
	"testing"
)

func Test_spin(t *testing.T) {
	tests := []struct {
		name   string
		input  programs
		number int
		want   string
	}{
		{"test1", programs{[]rune{'a', 'b', 'c', 'd', 'e'}, 0}, 3, "cdeab"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			spin(&tt.input, tt.number)
			if tt.input.String() != tt.want {
				t.Errorf("spin(%v, %v) = %v, want %v", tt.input, tt.number, tt.input.String(), tt.want)
			}
		})
	}
}

func Test_exchange(t *testing.T) {
	tests := []struct {
		name  string
		input programs
		a     int
		b     int
		want  string
	}{
		{"test1", programs{[]rune{'a', 'b', 'c', 'd', 'e'}, 0}, 2, 4, "abedc"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			exchange(&tt.input, tt.a, tt.b)
			if tt.input.String() != tt.want {
				t.Errorf("exchange(%v, %v, %v) = %v, want %v", tt.input, tt.a, tt.b, tt.input.String(), tt.want)
			}
		})
	}
}

func Test_partner(t *testing.T) {
	tests := []struct {
		name  string
		input programs
		a     rune
		b     rune
		want  string
	}{
		{"test1", programs{[]rune("abcde"), 4}, 'e', 'b', "baecd"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			partner(&tt.input, tt.a, tt.b)
			if tt.input.String() != tt.want {
				t.Errorf("partner(%v, %v, %v) = %v, want %v", tt.input, tt.a, tt.b, tt.input.String(), tt.want)
			}
		})
	}
}

func Test_parseInput(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{"test1", "s1,x3/4,pe/b", "baedc"},
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
		{"test1", "s1,x3/4,pe/b", 5, 2, "ceadb"},
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
