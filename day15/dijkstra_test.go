/*
Package dijkstra is an highly optimised implementation of the Dijkstra
algorithm, used for find the shortest path between points of a graph.

A graph is a map of points and map to the neighbouring points in the graph and
the cost to reach them.
A trivial example of a graph definition is:

	Graph{
		"a": {"b": 10, "c": 20},
		"b": {"a": 50},
		"c": {"b": 10, "a": 25},
	}

Borrowed and tweaked this implementation from https://github.com/albertorestifo/dijkstra

*/
package main

import "testing"

func Test_boolMatrix_set(t *testing.T) {
	type args struct {
		x int
		y int
		v bool
	}
	tests := []struct {
		name   string
		matrix boolMatrix
		args   []args
		want   string
	}{
		//{"x", newBoolMatrix(5, 4), []args{{1, 1, true}, {1, 2, true}, {4, 3, true}}, ".....\n.X...\n.X...\n....X\n"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			for _, args := range tt.args {
				tt.matrix.set(args.x, args.y, args.v)
			}
			if s := tt.matrix.String(); s != tt.want {
				t.Errorf("set(...) = %v want %v\n", s, tt.want)
			}
		})
	}
}
