package main

func main() {
	//println(runPart1())
	println(runPart2())
}

func runPart1() int {
	g1 := generator(618, 16807, 2147483647, 1)
	g2 := generator(814, 48271, 2147483647, 1)
	numMatching := 0
	for i := 0; i < 40000000; i++ {
		a, b := <-g1, <-g2
		if compareIntegers(a, b) {
			numMatching++
		}
	}
	return numMatching
}

func runPart2() int {
	g1 := generator(618, 16807, 2147483647, 4)
	g2 := generator(814, 48271, 2147483647, 8)
	numMatching := 0
	for i := 0; i < 5000000; i++ {
		a, b := <-g1, <-g2
		if compareIntegers(a, b) {
			numMatching++
		}
	}
	return numMatching
}

func generator(previousNumber int, factor int, dividor int, multiplePredicate int) <-chan int {
	ch := make(chan int, 10000)
	go func() {
		for {
			previousNumber = (previousNumber * factor) % dividor
			if multiplePredicate == 1 {
				ch <- previousNumber
			} else if previousNumber%multiplePredicate == 0 {
				ch <- previousNumber
			}
		}
	}()
	return ch
}

func compareIntegers(a int, b int) bool {
	bitMask := 0xffff
	// 16 bits
	for i := 1; i < 3; i++ {
		if a&bitMask != b&bitMask {
			return false
		}
	}
	return true
}
