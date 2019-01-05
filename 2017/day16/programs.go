package main

type programs struct {
	p     []rune
	start int
}

func (p *programs) get(i int) rune {
	return p.p[p.getRealIndex(i)]
}

func (p *programs) set(i int, value rune) {
	p.p[p.getRealIndex(i)] = value
}

func (p *programs) len() int {
	return len(p.p)
}

func (p *programs) moveStart(newStart int) {
	p.start = p.getRealIndex(newStart)
}

func (p *programs) getRealIndex(i int) int {
	attempt := p.start + i
	if attempt < p.len() {
		return attempt
	}
	return attempt - p.len()
}

func (p *programs) find(a ...rune) []int {
	output := make([]int, len(a))
	for i := 0; i < p.len(); i++ {
		realI := p.getRealIndex(i)
		for j, c := range a {
			if p.p[realI] == c {
				output[j] = i
			}
		}
	}
	return output
}

func (p *programs) update(newVal string) {
	p.p = []rune(newVal)
	p.start = 0
}

func (p *programs) String() string {
	output := make([]rune, p.len())
	for i := 0; i < p.len(); i++ {
		output[i] = p.get(i)
	}
	return string(output)
}

func getPrograms(number int) *programs {
	programs := new(programs)
	programs.p = make([]rune, number)
	for i := 0; i < number; i++ {
		programs.set(i, rune(int('a')+i))
	}
	return programs
}
