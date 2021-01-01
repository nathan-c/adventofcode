package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

type input struct {
	mask    *string
	address uint
	value   uint
}

func newMem(address, value uint) input {
	return input{address: address, value: value}
}

func newMask(mask string) input {
	return input{mask: &mask}
}

func main() {
	inputs := readInput()
	registers := make(map[uint]uint)
	mask := inputs[0].mask

	for _, line := range inputs[1:] {
		if line.mask != nil {
			mask = line.mask
		} else {
			registers[line.address] = apply(line.value, *mask)
		}
	}
	sum := uint(0)
	for _, val := range registers {
		sum += val
	}
	fmt.Printf("Part 1: %v\n", sum)
	fmt.Printf("Part 2: %v\n", part2(inputs))
}

func apply(value uint, mask string) uint {

	for i, bit := range mask {

		if bit == 'X' {
			continue
		}
		if bit == '1' {
			value |= (uint(1) << (35 - i))
		} else {
			value &= ^(uint(1) << (35 - i))
		}

	}
	return value
}

func part2(inputs []input) uint {

	registers := make(map[uint]uint)
	mask := inputs[0].mask
	inputs = inputs[1:]
	for _, line := range inputs {
		if line.mask != nil {
			mask = line.mask
		} else {
			addresses := applyVersion2(line.address, *mask)
			for _, address := range addresses {
				registers[address] = line.value
			}
		}
	}
	sum := uint(0)
	for _, val := range registers {
		sum += val
	}
	return sum
}

func applyVersion2(address uint, mask string) []uint {

	orMask := getOrMask(mask)
	address |= orMask
	addresses := []uint{address}

	for i, x := range mask {
		if x != 'X' {
			continue
		}
		length := len(addresses)
		for j := 0; j < length; j++ {
			val := addresses[j]
			addresses[j] = val | uint(1)<<(35-i)
			addresses = append(addresses, val&^(uint(1)<<(35-i)))
		}
	}

	return addresses
}

func getOrMask(mask string) uint {
	var runes []rune
	for _, r := range mask {
		if r == 'X' {
			runes = append(runes, '0')
		} else {
			runes = append(runes, r)
		}
	}
	maskInt, err := strconv.ParseUint(string(runes), 2, 36)
	if err != nil {
		panic(err)
	}
	return uint(maskInt)
}

func maskMultiplier2(mask string) []uint {
	masks := []uint{0}
	for i, x := range mask {
		y := (uint(1) << (35 - i))
		if x == 'X' {
			length := len(masks)
			for j := 0; j < length; j++ {
				val := masks[j]
				masks[j] += y
				masks = append(masks, val-y)
			}
		}
	}
	return masks
}

func readInput() []input {
	f, _ := os.Open("input")
	defer f.Close()

	var inputs []input

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		if line[:4] == "mask" {
			inputs = append(inputs, newMask(line[7:]))
		} else {
			var address uint
			var value uint
			fmt.Sscanf(line, "mem[%d] = %d", &address, &value)
			inputs = append(inputs, newMem(address, value))
		}
	}
	return inputs
}
