package main

import "testing"

func Test_attemptFirewall(t *testing.T) {
	tests := []struct {
		name string
		args map[int]int
		want int
	}{
		{name: "test1", args: map[int]int{0: 3, 1: 2, 4: 4, 6: 4}, want: 10},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := attemptFirewall(tt.args); got != tt.want {
				t.Errorf("attemptFirewall() = %v, want %v", got, tt.want)
			}
		})
	}
}
