package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

func main() {
	allRanges, fields, allTickets := readInput()
	myTicket := allTickets[0]

	var validTickets [][]int
	invalidSum := 0
	for _, ticket := range allTickets[1:] {
		ticketValid := true
		for _, val := range ticket {
			valid := false
			for _, r := range allRanges {
				if val >= r[0] && val <= r[1] {
					valid = true
					break
				}
			}
			if !valid {
				invalidSum += val
				ticketValid = false
			}
		}
		if ticketValid {
			validTickets = append(validTickets, ticket)
		}
	}
	validTickets = append(validTickets, myTicket)
	fmt.Printf("Part 1: %v\n", invalidSum)
	fieldCount := len(fields)
	var allPossibleFields []*possibleField
	for i := 0; i < fieldCount; i++ {
		possibleFields := make([]field, fieldCount)
		copy(possibleFields, fields)
		for _, ticket := range validTickets {
			val := ticket[i]
			for j := len(possibleFields) - 1; j >= 0; j-- {
				field := possibleFields[j]
				if (field.ranges[0][0] > val || field.ranges[0][1] < val) &&
					(field.ranges[1][0] > val || field.ranges[1][1] < val) {
					possibleFields = append(possibleFields[:j], possibleFields[j+1:]...)
				}
			}
		}
		allPossibleFields = append(allPossibleFields, &possibleField{i, possibleFields})
	}

	sort.SliceStable(allPossibleFields, func(i, j int) bool {
		iVal := allPossibleFields[i]
		jVal := allPossibleFields[j]
		return len(iVal.fields) < len(jVal.fields)
	})
	for _, possibleField := range allPossibleFields {
		if len(possibleField.fields) == 1 {
			name := possibleField.fields[0].name
			for _, possibleField2 := range allPossibleFields {
				if possibleField.index == possibleField2.index {
					continue
				}
				newFields := possibleField2.fields[:0]
				for _, field := range possibleField2.fields {
					if field.name != name {
						newFields = append(newFields, field)
					}
				}
				possibleField2.fields = newFields
			}
		} else {
			panic("urgh")
		}
	}
	orderedFields := make([]field, fieldCount)
	for _, possibleField := range allPossibleFields {
		if len(possibleField.fields) != 1 {
			panic("oops")
		}
		orderedFields[possibleField.index] = possibleField.fields[0]
	}
	result := 1
	prefix := "departure"
	prefixLength := len(prefix)
	for i, value := range myTicket {
		field := orderedFields[i]
		if len(field.name) > prefixLength && field.name[:prefixLength] == prefix {
			result *= value
		}
	}
	fmt.Printf("Part 2: %v\n", result)
}

type possibleField struct {
	index  int
	fields []field
}

type field struct {
	name   string
	ranges [][2]int
}

func readInput() ([][2]int, []field, [][]int) {
	f, _ := os.Open("input")
	defer f.Close()

	headers := true
	var allRanges [][2]int
	var fields []field
	var allTickets [][]int

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			headers = false
			continue
		}
		if headers {
			field := getField(line)
			allRanges = append(allRanges, field.ranges...)
			fields = append(fields, field)
		} else {
			split := strings.Split(line, ",")
			if len(split) < 2 {
				continue
			}
			vals := toInts(split)
			allTickets = append(allTickets, vals)
		}
	}
	return allRanges, fields, allTickets
}

func getField(line string) field {
	var fieldRanges [][2]int
	split := strings.Split(line, ": ")
	name := split[0]
	ranges := split[1]
	split = strings.Split(ranges, " or ")
	for _, r := range split {
		splitR := strings.Split(r, "-")
		start, err1 := strconv.Atoi(splitR[0])
		if err1 != nil {
			panic(err1)
		}
		end, err2 := strconv.Atoi(splitR[1])
		if err2 != nil {
			panic(err2)
		}
		fieldRanges = append(fieldRanges, [2]int{start, end})
	}
	return field{name, fieldRanges}
}

func toInts(stringSlice []string) []int {
	intSlice := make([]int, len(stringSlice))
	for i, x := range stringSlice {
		intSlice[i], _ = strconv.Atoi(x)
	}
	return intSlice
}
