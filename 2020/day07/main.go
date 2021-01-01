package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type rule map[string]int

type rules map[string]rule

func main() {
	f, _ := os.Open("input")
	defer f.Close()

	allRules := make(rules)
	allWrappers := make(map[string]map[string]bool)

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		line = strings.ReplaceAll(line, "bags", "bag")
		split := strings.Split(line, " bag contain ")
		if len(split) != 2 {
			panic(line)
		}
		if split[1] == "no other bag." {
			allRules[split[0]] = make(rule)
			continue
		}
		lineRulesString := strings.Split(split[1], ", ")
		lineRules := make(rule)
		allRules[split[0]] = lineRules
		for _, lineRuleString := range lineRulesString {
			var count int
			var bag1 string
			var bag2 string
			_, err := fmt.Sscanf(lineRuleString, "%d %s %s bag", &count, &bag1, &bag2)
			if err != nil {
				panic(fmt.Errorf("%v, %v", lineRuleString, err))
			}
			bag := bag1 + " " + bag2
			lineRules[bag] = count
			var wrappers map[string]bool
			var ok bool
			if wrappers, ok = allWrappers[bag]; !ok {
				wrappers = make(map[string]bool)
				allWrappers[bag] = wrappers
			}
			wrappers[split[0]] = true
		}
	}
	shinyGoldWrappers := make(map[string]bool)
	toCount := []string{"shiny gold"}
	for len(toCount) > 0 {
		item := toCount[0]
		toCount = toCount[1:]
		if wrappers, ok := allWrappers[item]; ok {
			toCount = append(toCount, keys(wrappers)...)
		}
		if item != "shiny gold" {
			shinyGoldWrappers[item] = true
		}
	}

	fmt.Printf("Part 1: %v\n", len(shinyGoldWrappers))

	flattenedContents := make(map[string]int)
	count := allRules.getAllContents("shiny gold", flattenedContents, 1)
	fmt.Printf("Part 2: %v\n", count)
}

func (r rules) getAllContents(bag string, flattenedContents map[string]int, multiplier int) int {
	contents := r[bag]
	count := 0
	for k, v := range contents {
		flattenedContents[k] += v
		count += v * multiplier
		count += r.getAllContents(k, flattenedContents, v*multiplier)
	}
	return count
}

func keys(dict map[string]bool) []string {
	keys := make([]string, len(dict))

	i := 0
	for k := range dict {
		keys[i] = k
		i++
	}
	return keys
}
