package main

import (
	"reflect"
	"testing"
)

func Test_removeDeadGroups(t *testing.T) {

	tests := []struct {
		name   string
		groups []*group
		want   []*group
	}{
		{"empty", []*group{}, []*group{}},
		{"nothing changed", []*group{&group{units: 1}}, []*group{&group{units: 1}}},
		{"last removed", []*group{&group{units: 0}}, []*group{}},
		{"one removed", []*group{&group{units: 0}, &group{units: 1}}, []*group{&group{units: 1}}},
		{"one removed 2", []*group{&group{units: 2}, &group{units: 0}, &group{units: 1}}, []*group{&group{units: 2}, &group{units: 1}}},
		{"all removed", []*group{&group{units: -17}, &group{units: -12}}, []*group{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := removeDeadGroups(tt.groups); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("removeDeadGroups() = %v, want %v", got, tt.want)
			}
		})
	}
}
