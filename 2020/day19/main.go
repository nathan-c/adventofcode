package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	rules, messages := readInput()
	rule0 := rules.getStringVals(0)
	rule0Map := make(map[string]bool)
	for _, match := range rule0 {
		rule0Map[match] = true
	}
	countP1 := 0
	for _, msg := range messages {
		if _, ok := rule0Map[msg]; ok {
			countP1++
		}
	}
	fmt.Printf("Part 1: %v\n", countP1)

	/*
	   0: 8 11 -> this is 42n+42m+31m
	   8: 42 | 42 8 -> this is 42n
	   11: 42 31 | 42 11 31 -> this is 42n+31n
	*/

	rule31 := rules.getStringVals(31)
	// fmt.Println(rule31)
	rule42 := rules.getStringVals(42)
	// fmt.Println(rule42)
	countP2 := 0
	for _, msg := range messages {
		count42s := 0
		count31s := 0
		// var matched42s []string
		// var matched31s []string
		part := msg
		for len(part) > 0 {
			found := false
			if count31s == 0 {
				for _, r42 := range rule42 {
					if part[:len(r42)] == r42 {
						count42s++
						// matched42s = append(matched42s, r42)
						part = part[len(r42):]
						found = true
						break
					}
				}
			}
			if !found && count42s > 0 {
				for _, r31 := range rule31 {
					if part[:len(r31)] == r31 {
						count31s++
						// matched31s = append(matched31s, r31)
						found = true
						part = part[len(r31):]
						break
					}
				}
			}
			if !found {
				break
			}
		}

		if len(part) == 0 && count42s > 0 && count31s > 0 && count42s > count31s {

			// if strings.Join(matched42s, "")+strings.Join(matched31s, "") != msg {
			// 	panic(msg)
			// }
			countP2++
		}
	}
	fmt.Printf("Part 2: %v\n", countP2)
}

func readInput() (rules, []string) {

	f, _ := os.Open("input")
	defer f.Close()

	ruleMap := make(map[int]string)
	maxIndex := 0
	var messages []string

	scanner := bufio.NewScanner(f)
	inRules := true
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			inRules = false
		}
		if inRules {
			split := strings.Split(line, ": ")
			index, err := strconv.Atoi(split[0])
			if err != nil {
				panic(err)
			}
			ruleMap[index] = split[1]
			if index > maxIndex {
				maxIndex = index
			}
		} else {
			messages = append(messages, line)
		}
	}
	rulesRaw := make([]string, maxIndex+1)
	for k, v := range ruleMap {
		rulesRaw[k] = v
	}
	return rules{raw: rulesRaw, flatRules: make([][]string, len(rulesRaw))}, messages
}

type rules struct {
	raw       []string
	parsed    []interface{}
	flatRules [][]string
}

func (r *rules) parseRules() []interface{} {
	if r.parsed != nil {
		return r.parsed
	}
	parsedRules := make([]interface{}, len(r.raw))
	for i, rawRule := range r.raw {
		if len(rawRule) == 0 {
			continue
		}
		parsedRules[i] = parseRule(rawRule)
	}
	r.parsed = parsedRules
	return parsedRules
}

func (r *rules) flatten() []string {
	if r.flatRules == nil {
		r.flatRules = make([][]string, len(r.raw))
	}

	var allValidMessages []string
	for i := 0; i < len(r.raw); i++ {
		allValidMessages = append(allValidMessages, r.getStringVals(i)...)
	}
	return allValidMessages
}

func (r *rules) getStringVals(idx int) []string {

	if r.flatRules != nil && r.flatRules[idx] != nil {
		return r.flatRules[idx]
	}

	var flattenedRule []string
	parsedRules := r.parseRules()
	rule := parsedRules[idx]
	if char, ok := rule.(rune); ok {
		flattenedRule = []string{string(char)}
	} else if compositeRule, ok := rule.([][]int); ok {
		for _, opt := range compositeRule {
			combs := make([][]string, len(opt))
			for i, subIdx := range opt {
				subVals := r.getStringVals(subIdx)
				for _, subVal := range subVals {
					combs[i] = append(combs[i], subVal)
				}
			}
			if len(combs) == 1 {
				flattenedRule = append(flattenedRule, combs[0]...)
			} else if len(combs) == 2 {
				for _, a := range combs[0] {
					for _, b := range combs[1] {
						flattenedRule = append(flattenedRule, a+b)
					}
				}
			} else if len(combs) == 3 {
				for _, a := range combs[0] {
					for _, b := range combs[1] {
						for _, c := range combs[2] {
							flattenedRule = append(flattenedRule, a+b+c)
						}
					}
				}
			} else {
				panic("nooooo")
			}
		}
	} else {
		panic("oop")
	}
	r.flatRules[idx] = flattenedRule
	return flattenedRule
}

func parseRule(rule string) interface{} {
	if rule[0] == '"' {
		return rune(rule[1])
	}
	var parsedRules [][]int
	split := strings.Split(rule, " | ")
	for _, r := range split {
		rSplit := strings.Split(r, " ")
		var idxs []int
		for _, idxStr := range rSplit {
			idx, err := strconv.Atoi(idxStr)
			if err != nil {
				panic(err)
			}
			idxs = append(idxs, idx)
		}
		parsedRules = append(parsedRules, idxs)
	}
	return parsedRules
}
