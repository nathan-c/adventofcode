package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
	"strings"
	"sync"
)

type particle struct {
	p  [3]int
	v  [3]int
	a  [3]int
	id int
}

func (part *particle) moveOne() {
	for i := 0; i < 3; i++ {
		part.v[i] += part.a[i]
	}
	for i := 0; i < 3; i++ {
		part.p[i] += part.v[i]
	}
}

func (part *particle) distance() int {
	return abs(part.p[0]) + abs(part.p[1]) + abs(part.p[2])
}

func (part *particle) String() string {
	return fmt.Sprint(part.distance())
}

func runPart1(particles []*particle, iterations int) int {
	var wg sync.WaitGroup
	wg.Add(len(particles))
	for _, part := range particles {
		go func(p *particle) {
			defer wg.Done()
			for i := 0; i < iterations; i++ {
				p.moveOne()
				//fmt.Printf("%v, %v, %v, %v\r", p.id, i, p.distance(), p)
			}
		}(part)
	}
	wg.Wait()
	minI := 0
	minD := particles[0].distance()
	for _, part := range particles {
		if d := part.distance(); d < minD {
			minI = part.id
			minD = d
		}
		//fmt.Printf("%v, %v, %v\r", part.id, part.distance(), part)
	}
	return minI
}
func runPart2(particles []*particle, iterations int) int {
	for i := 0; i < iterations; i++ {
		var wg sync.WaitGroup
		wg.Add(len(particles))
		for _, part := range particles {
			go func(p *particle) {
				defer wg.Done()
				p.moveOne()

			}(part)
		}
		wg.Wait()
		taken := make(map[[3]int]bool)
		toRemove := make(map[[3]int]bool)
		for _, part := range particles {
			if _, in := taken[part.p]; !in {
				taken[part.p] = true
			} else {
				toRemove[part.p] = true
			}
		}
		var newParticles []*particle
		for _, part := range particles {
			if _, in := toRemove[part.p]; !in {
				newParticles = append(newParticles, part)
			}
		}
		particles = newParticles
	}

	return len(particles)
}

func main() {
	particles := parseInput("day20.input.txt")
	println(runPart1(particles, 10000))
	particles = parseInput("day20.input.txt")
	println(runPart2(particles, 10000))
}

func parseInput(fileName string) []*particle {
	myExp := regexp.MustCompile(`p=<(?P<p0>(|-)\d+),(?P<p1>(|-)\d+),(?P<p2>(|-)\d+)>, v=<(?P<v0>(|-)\d+),(?P<v1>(|-)\d+),(?P<v2>(|-)\d+)>, a=<(?P<a0>(|-)\d+),(?P<a1>(|-)\d+),(?P<a2>(|-)\d+)>`)

	var particles []*particle
	dat, _ := ioutil.ReadFile(fileName)
	stringFile := string(dat)
	lines := strings.Split(stringFile, "\n")
	for id, line := range lines {
		matches := getMatches(myExp, line)
		p := particle{
			[3]int{matches["p0"], matches["p1"], matches["p2"]},
			[3]int{matches["v0"], matches["v1"], matches["v2"]},
			[3]int{matches["a0"], matches["a1"], matches["a2"]},
			id,
		}
		particles = append(particles, &p)
	}
	return particles
}

func getMatches(myExp *regexp.Regexp, text string) map[string]int {
	match := myExp.FindStringSubmatch(text)
	result := make(map[string]int)
	for i, name := range myExp.SubexpNames() {
		if i != 0 {
			val, _ := strconv.Atoi(match[i])
			result[name] = val
		}
	}
	return result
}

func abs(a int) int {
	if a < 0 {
		return -a
	}
	return a
}
