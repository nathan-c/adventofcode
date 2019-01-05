package main

import (
	"fmt"
	"reflect"
	"testing"
)

func Test_bfsMove(t *testing.T) {

	// generates tests
	part2("input.txt", 4)
	//t.Errorf("%v", len(inputs))
	for i, tt := range inputs {
		t.Run(fmt.Sprint(i), func(t *testing.T) {
			m, _ := parseInputFromString(tt.u, 0)
			want := outputs[i]
			got, got1 := __bfsMove(m, tt.l, tt.enemyType)

			if !reflect.DeepEqual(got, want.l) {
				t.Errorf("__bfsMove() got = %v, want %v", got, want.l)
			}
			if got1 != outputs[i].moved {
				t.Errorf("__bfsMove() got1 = %v, want %v", got1, want.moved)
			}
		})
	}
}
