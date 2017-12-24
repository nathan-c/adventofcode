package main

import (
	"io/ioutil"
	"strconv"
	"strings"
)

func scanner(height int, time int) int {
	offset := time % ((height - 1) * 2)
	if offset > height-1 {
		return 2*(height-1) - offset
	}
	return offset
}

func attemptFirewall(firewall map[int]int) int {
	for i := 0; ; i++ {
		full := true
		for k, v := range firewall {
			if scanner(v, k+i) == 0 {
				full = false
				break
			}
		}
		if full {
			return i
		}
	}
}

func main() {
	firewall := make(map[int]int)
	dat, _ := ioutil.ReadFile("day13.input.txt")
	stringFile := string(dat)
	lines := strings.Split(stringFile, "\n")
	for _, line := range lines {
		splitLine := strings.Split(line, ": ")
		if len(splitLine) != 2 {
			continue
		}
		key, _ := strconv.Atoi(splitLine[0])
		val, _ := strconv.Atoi(splitLine[1])
		firewall[key] = val
	}
	print(attemptFirewall(firewall))
}
