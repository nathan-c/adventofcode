package main

import "fmt"

func main() {
	a := 11349501
	b := 5107328
	//a := 17807724
	//b := 5764801

	fmt.Println(transform(7, 8))
	fmt.Println(transform(7, 11))

	aVal := 1
	aLoopSize := 1
	for ; ; aLoopSize++ {
		aVal = incrementTransform(7, aVal)
		if aVal == a {
			break
		}
	}
	bVal := 1
	bLoopSize := 1
	for ; ; bLoopSize++ {
		bVal = incrementTransform(7, bVal)
		if bVal == b {
			break
		}
	}

	aEnc := transform(a, bLoopSize)
	bEnc := transform(b, aLoopSize)
	fmt.Println(aEnc, bEnc)
}

func transform(sn, loopSize int) int {
	value := 1
	for i := 0; i < loopSize; i++ {
		value = incrementTransform(sn, value)
	}
	return value
}

func incrementTransform(sn, value int) int {
	value = (value * sn) % 20201227
	return value
}
