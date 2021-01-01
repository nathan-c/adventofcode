package main

import (
	"bufio"
	"log"
	"os"
	"strconv"
	"strings"
	"unicode"
)

// byr (Birth Year) - four digits; at least 1920 and at most 2002.
// iyr (Issue Year) - four digits; at least 2010 and at most 2020.
// eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
// hgt (Height) - a number followed by either cm or in:
// If cm, the number must be at least 150 and at most 193.
// If in, the number must be at least 59 and at most 76.
// hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
// ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
// pid (Passport ID) - a nine-digit number, including leading zeroes.
// cid (Country ID) - ignored, missing or not.

var requiredFields map[string]func(value string) bool = map[string]func(value string) bool{
	"byr": func(v string) bool { return yearValidation(v, 1920, 2002) },
	"iyr": func(v string) bool { return yearValidation(v, 2010, 2020) },
	"eyr": func(v string) bool { return yearValidation(v, 2020, 2030) },
	"hgt": length,
	"hcl": colour,
	"ecl": eyeColour,
	"pid": pid,
	//"cid",
}

func yearValidation(value string, minYr, maxYr int) bool {

	year, err := strconv.Atoi(value)
	if err != nil {
		return false
	}
	if year >= minYr && year <= maxYr {
		return true
	}
	return false
}

func length(value string) bool {
	numberStr := string(value[:len(value)-2])

	number, err := strconv.Atoi(numberStr)
	if err != nil {
		return false
	}

	units := string(value[len(value)-2:])
	if units == "cm" && number >= 150 && number <= 193 {
		return true
	}
	if units == "in" && number >= 59 && number <= 76 {
		return true
	}

	return false
}

func colour(value string) bool {

	if rune(value[0]) != '#' {
		return false
	}
	_, err := strconv.ParseUint(value[1:], 16, 64)
	if err != nil {
		return false
	}

	return true
}

var eyeColours []string = []string{
	"amb", "blu", "brn", "gry", "grn", "hzl", "oth",
}

func eyeColour(value string) bool {
	for _, colour := range eyeColours {
		if colour == value {
			return true
		}
	}
	return false
}

func pid(value string) bool {
	if len(value) != 9 {
		return false
	}
	for _, c := range value {
		if !unicode.IsNumber(c) {
			return false
		}
	}
	return true
}

func main() {
	f, _ := os.Open("input")
	defer f.Close()

	scanner := bufio.NewScanner(f)
	scanner.Split(ScanDoubleLines)

	count := 0

	for scanner.Scan() {
		entryString := string(scanner.Bytes())
		entry := strings.Fields(entryString)
		passport := make(map[string]string)
		for _, item := range entry {
			kvp := strings.Split(item, ":")
			passport[kvp[0]] = kvp[1]
		}

		valid := true
		for item, validation := range requiredFields {
			if val, ok := passport[item]; ok && validation(val) {
			} else {
				valid = false
			}
		}
		if valid {
			count++
		}
	}

	log.Println(count)
}

func ScanDoubleLines(data []byte, atEOF bool) (advance int, token []byte, err error) {
	if atEOF && len(data) == 0 {
		return 0, nil, nil
	}

	if i := strings.Index(string(data), "\n\n"); i >= 0 {
		// We have a full newline-terminated line.
		return i + 1, data[0:i], nil
	}
	// If we're at EOF, we have a final, non-terminated line. Return it.
	if atEOF {
		return len(data), data, nil
	}
	// Request more data.
	return 0, nil, nil
}
