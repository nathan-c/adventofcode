package main

type instruction struct {
	i       func([]int, int, int, int)
	a, b, c int
}

func buildInstructionMap() map[string]func([]int, int, int, int) {
	return map[string]func([]int, int, int, int){
		"addr": addr,
		"addi": addi,
		"mulr": mulr,
		"muli": muli,
		"banr": banr,
		"bani": bani,
		"borr": borr,
		"bori": bori,
		"setr": setr,
		"seti": seti,
		"gtir": gtir,
		"gtri": gtri,
		"gtrr": gtrr,
		"eqir": eqir,
		"eqri": eqri,
		"eqrr": eqrr,
	}
}

func addr(r []int, a, b, c int) {
	r[c] = r[a] + r[b]
}

func addi(r []int, a, b, c int) {
	r[c] = r[a] + b
}

func mulr(r []int, a, b, c int) {
	r[c] = r[a] * r[b]
}
func muli(r []int, a, b, c int) {
	r[c] = r[a] * b
}

func banr(r []int, a, b, c int) {
	r[c] = r[a] & r[b]
}

func bani(r []int, a, b, c int) {
	r[c] = r[a] & b
}

func borr(r []int, a, b, c int) {
	r[c] = r[a] | r[b]
}

func bori(r []int, a, b, c int) {
	r[c] = r[a] | b
}

func setr(r []int, a, b, c int) {
	r[c] = r[a]
}

func seti(r []int, a, b, c int) {
	r[c] = a
}

func gtir(r []int, a, b, c int) {
	if a > r[b] {
		r[c] = 1
	} else {
		r[c] = 0
	}
}

func gtri(r []int, a, b, c int) {
	if r[a] > b {
		r[c] = 1
	} else {
		r[c] = 0
	}
}

func gtrr(r []int, a, b, c int) {
	if r[a] > r[b] {
		r[c] = 1
	} else {
		r[c] = 0
	}
}

func eqir(r []int, a, b, c int) {
	if a == r[b] {
		r[c] = 1
	} else {
		r[c] = 0
	}
}

func eqri(r []int, a, b, c int) {
	if r[a] == b {
		r[c] = 1
	} else {
		r[c] = 0
	}
}

func eqrr(r []int, a, b, c int) {
	if r[a] == r[b] {
		r[c] = 1
	} else {
		r[c] = 0
	}
}
