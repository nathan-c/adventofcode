package main

func main() {
	index := 0
	itemAfter0 := 0
	for i := 1; i < 50000001; i++ {
		index = (371+index)%i + 1
		if index == 1 {
			itemAfter0 = i
		}
	}
	println(itemAfter0)
}
