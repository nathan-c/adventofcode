package main

import (
	"bufio"
	"log"
	"os"
	"strconv"
)

func main() {
	file, err := os.Open("input")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	inputSet := make(map[int]int)
	count := 0

	for scanner.Scan() {
		i, err := strconv.Atoi(scanner.Text())
		if err != nil {
			log.Fatal(err)
		}
		inputSet[i] = 2020 - i
		count++
	}

	if count != len(inputSet) {
		log.Fatal("duplicate entries in input")
	}

	log.Println("Part 1:")
	for key, value := range inputSet {
		if _, ok := inputSet[value]; ok {
			log.Printf("%v * %v = %v", value, key, value*key)
			break
		}
	}

	log.Println("Part 2:")
	func() {
		for key1, value := range inputSet {
			for key2 := range inputSet {
				needed := value - key2
				if _, ok := inputSet[needed]; ok {
					log.Printf("%v * %v * %v = %v", needed, key1, key2, needed*key1*key2)
					return
				}
			}
		}
	}()
}
