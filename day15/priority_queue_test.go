// // This example demonstrates a priority queue built using the heap interface.
package main

// import (
// 	"reflect"
// 	"sort"
// 	"testing"
// )

// func Test_newPriorityQueue(t *testing.T) {
// 	tests := []struct {
// 		name  string
// 		input []int
// 	}{
// 		{"test", []int{12, 22, 3, 14, 5, 16, 27, 8, 50}},
// 	}
// 	for _, tt := range tests {
// 		t.Run(tt.name, func(t *testing.T) {
// 			want := make([]int, len(tt.input))
// 			copy(want, tt.input)
// 			sort.Ints(want)
// 			pq := newPriorityQueue()
// 			for _, i := range tt.input {
// 				pq.Push(location{i, i}, i)
// 			}
// 			got := []int{}
// 			for pq.Len() > 0 {
// 				_, i := pq.Pop()
// 				got = append(got, i)
// 			}
// 			if !reflect.DeepEqual(got, want) {
// 				t.Errorf("newPriorityQueue() = %v, want %v", got, want)
// 			}
// 		})
// 	}
// }

// func TestPriorityQueueData_Pop(t *testing.T) {

// 	tests := []struct {
// 		name   string
// 		fields []int
// 		want   int
// 	}{
// 		{"test1", []int{1, 2, 3}, 2},
// 	}
// 	for _, tt := range tests {
// 		t.Run(tt.name, func(t *testing.T) {
// 			pq := newPriorityQueue()
// 			for _, i := range tt.fields {
// 				pq.Push(location{i, i}, i)
// 				if l := pq.d.Len(); l != i {
// 					t.Errorf("Push: PriorityQueueData.Len() = %v, want %v", l, i)
// 				}
// 				if l := len(pq.d.items); l != i {
// 					t.Errorf("Push: PriorityQueueData.Len() = %v, want %v", l, i)
// 				}
// 				if l := len(pq.d.nodes); l != i {
// 					t.Errorf("Push: PriorityQueueData.Len() = %v, want %v", l, i)
// 				}
// 			}
// 			length := pq.Len()
// 			for pq.Len() > 0 {
// 				if l := pq.d.Len(); l != length {
// 					t.Errorf("Pop: PriorityQueueData.Len() = %v, want %v", l, length)
// 				}
// 				if l := len(pq.d.items); l != length {
// 					t.Errorf("Pop: PriorityQueueData.Len() = %v, want %v", l, length)
// 				}
// 				if l := len(pq.d.nodes); l != length {
// 					t.Errorf("Pop: PriorityQueueData.Len() = %v, want %v", l, length)
// 				}
// 				pq.Pop()
// 				length--
// 			}
// 		})
// 	}
// }

// func TestPriorityQueue_Pop(t *testing.T) {
// 	type fields struct {
// 		d *PriorityQueueData
// 	}
// 	tests := []struct {
// 		name         string
// 		fields       fields
// 		wantValue    location
// 		wantPriority int
// 	}{
// 		// TODO: Add test cases.
// 	}
// 	for _, tt := range tests {
// 		t.Run(tt.name, func(t *testing.T) {
// 			pq := &PriorityQueue{
// 				d: tt.fields.d,
// 			}
// 			gotValue, gotPriority := pq.Pop()
// 			if !reflect.DeepEqual(gotValue, tt.wantValue) {
// 				t.Errorf("PriorityQueue.Pop() gotValue = %v, want %v", gotValue, tt.wantValue)
// 			}
// 			if gotPriority != tt.wantPriority {
// 				t.Errorf("PriorityQueue.Pop() gotPriority = %v, want %v", gotPriority, tt.wantPriority)
// 			}
// 		})
// 	}
// }
