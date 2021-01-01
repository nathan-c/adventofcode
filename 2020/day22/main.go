package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	players := readInput()
	fmt.Println(players)
	partOne(players)
	players = readInput()
	partTwo(players)
}

func partTwo(players [2][]int) {
	_, winner := recursiveCombat(players)
	score := 0
	for i := 1; i <= len(winner); i++ {
		score += winner[len(winner)-i] * i
	}
	fmt.Printf("Part 2: %v\n", score)

}

func recursiveCombat(players [2][]int) (int, []int) {

	previousRounds := make(map[string]bool)

	for len(players[0]) != 0 && len(players[1]) != 0 {
		round := roundKey(players)
		if _, ok := previousRounds[round]; ok {
			return 0, players[0]
		}
		previousRounds[round] = true
		var winner int
		if players[0][0] < len(players[0]) && players[1][0] < len(players[1]) {
			newGame := createNewGame(players)
			winner, _ = recursiveCombat(newGame)
		} else {
			if players[0][0] > players[1][0] {
				winner = 0
			} else {
				winner = 1
			}
		}
		loser := (winner + 1) % 2
		players[winner] = append(players[winner][1:], players[winner][0], players[loser][0])
		players[loser] = players[loser][1:]
	}

	if len(players[0]) == 0 {
		return 1, players[1]
	}
	return 0, players[0]
}

func roundKey(players [2][]int) string {
	var sb strings.Builder
	for i := 0; i < 2; i++ {
		sb.WriteString("Player")
		sb.WriteString(fmt.Sprint(i))
		sb.WriteRune(':')
		for _, x := range players[i] {
			sb.WriteString(fmt.Sprint(x))
			sb.WriteRune(',')
		}
		sb.WriteRune('\n')
	}
	key := sb.String()
	return key
}

func createNewGame(players [2][]int) [2][]int {
	return [2][]int{createNewPlayer(players[0]), createNewPlayer(players[1])}
}

func createNewPlayer(player []int) []int {
	length := player[0]
	newGame := make([]int, length)
	copy(newGame, player[1:length+1])
	return newGame
}

func partOne(players [2][]int) {
	for len(players[0]) != 0 && len(players[1]) != 0 {
		if players[0][0] > players[1][0] {
			players[0] = append(players[0][1:], players[0][0], players[1][0])
			players[1] = players[1][1:]
		} else {
			players[1] = append(players[1][1:], players[1][0], players[0][0])
			players[0] = players[0][1:]
		}
	}

	var winner []int
	if len(players[0]) == 0 {
		winner = players[1]
	} else {
		winner = players[0]
	}
	score := 0
	for i := 1; i <= len(winner); i++ {
		score += winner[len(winner)-i] * i
	}
	fmt.Printf("Part 1: %v\n", score)

}

func readInput() [2][]int {

	f, _ := os.Open("input")
	defer f.Close()

	var players [2][]int
	player := -1

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}
		if len(line) > 6 && line[:6] == "Player" {
			player++
		} else {
			card, err := strconv.Atoi(line)
			if err != nil {
				panic(err)
			}
			players[player] = append(players[player], card)
		}
	}

	return players
}
