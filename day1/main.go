package main

import (
	"bufio"
	"log"
	"os"
	"strconv"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	sum := 0
	for scanner.Scan() { // internally, it advances token based on sperator
		i, err := strconv.Atoi(scanner.Text()) // token in unicode-char
		if err != nil {
			log.Fatal(err)
		}
		sum += i
	}
	println(sum)
}
