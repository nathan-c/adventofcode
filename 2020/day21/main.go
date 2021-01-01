package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"
)

func main() {
	foods, allergenMap, _ := readInput()
	fmt.Println(foods)

	allergens := make([]allergenToFoods, 0, len(allergenMap))

	for allergen, foods := range allergenMap {
		allergens = append(allergens, allergenToFoods{allergen, foods})
	}

	sort.Slice(allergens, func(i, j int) bool {
		x := allergens[i]
		y := allergens[j]
		return len(x.foods) > len(y.foods)
	})

	// map from ingredient to allergen that it might be
	possibleIngreds := make(map[string][]string)

	for _, kvp := range allergens {
		seen := make(map[string]int)
		for _, food := range kvp.foods {
			for _, ingred := range food.ingreds {
				seen[ingred]++
			}
		}

		for ingred, count := range seen {
			if count < len(kvp.foods) {
				continue
			}
			possibleIngreds[ingred] = append(possibleIngreds[ingred], kvp.allergen)
		}
	}

	for _, food := range foods {
		var unknownIngredients []string
		var possibleAllergens []string
		for _, ingred := range food.ingreds {
			if allergens := possibleIngreds[ingred]; len(allergens) != 0 {
				for _, allergen := range food.allergens {
					if !contains(allergens, allergen) {
						possibleAllergens = append(possibleAllergens, allergen)
					}
				}
			} else {
				unknownIngredients = append(unknownIngredients, ingred)
			}
		}
		if len(unknownIngredients) == len(possibleAllergens) {
			for _, ingred := range unknownIngredients {
				possibleIngreds[ingred] = possibleAllergens
			}
		}

	}

	count := 0
	for _, food := range foods {
		for _, ingred := range food.ingreds {
			if len(possibleIngreds[ingred]) == 0 {
				count++
			}
		}
	}

	fmt.Printf("Part 1: %v\n", count)

	knownAllergens := make(map[string]string)

	for len(possibleIngreds) > 0 {
		for ingred, allergens := range possibleIngreds {

			var unknown []string
			for _, allergen := range allergens {
				if _, ok := knownAllergens[allergen]; !ok {
					unknown = append(unknown, allergen)
				}
			}

			if len(unknown) == 1 {
				knownAllergens[unknown[0]] = ingred
			}
		}
		for _, ingred := range knownAllergens {
			delete(possibleIngreds, ingred)
		}
	}

	knownAllergenList := make([][2]string, 0, len(knownAllergens))
	for allergen, ingred := range knownAllergens {
		knownAllergenList = append(knownAllergenList, [2]string{allergen, ingred})
	}

	sort.Slice(knownAllergenList, func(i, j int) bool {
		x := knownAllergenList[i]
		y := knownAllergenList[j]
		return strings.Compare(x[0], y[0]) == -1
	})

	output := ""
	for _, x := range knownAllergenList {
		output += "," + x[1]
	}

	fmt.Printf("Part 2: %v\n", output[1:])
}

func contains(slice []string, x string) bool {
	for _, y := range slice {
		if x == y {
			return true
		}
	}
	return false
}

func readInput() ([]*food, map[string][]*food, map[string]bool) {

	f, _ := os.Open("input")
	defer f.Close()

	var foods []*food
	allergenMap := make(map[string][]*food)
	ingredientSet := make(map[string]bool)

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		split := strings.Split(line, " (contains ")
		ingreds := strings.Split(split[0], " ")
		var allergens []string
		if len(split) == 2 {
			allergens = strings.Split(split[1][:len(split[1])-1], ", ")
		}
		food := &food{ingreds, allergens}
		foods = append(foods, food)
		for _, allergen := range allergens {
			allergenMap[allergen] = append(allergenMap[allergen], food)
		}
		for _, i := range ingreds {
			ingredientSet[i] = true
		}
	}

	return foods, allergenMap, ingredientSet
}

type food struct {
	ingreds   []string
	allergens []string
}

type allergenToFoods struct {
	allergen string
	foods    []*food
}
