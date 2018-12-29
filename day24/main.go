package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

var (
	rx                  = `(\d+) units each with (\d+) hit points (\(.+\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)`
	weaknessImunitiesRx = `(weak|immune) to ([\w ,]+)`
)

func main() {
	immuneSystem, infection := parseInput("input.txt")

	immuneSystemCopy := boostAll(immuneSystem, 0)
	infectionCopy := boostAll(infection, 0)

	winningTeamsUnitCount, immuneWins := part1(immuneSystemCopy, infectionCopy)
	if immuneWins {
		fmt.Printf("part one: immune wins by %v\n", winningTeamsUnitCount)
	} else {
		fmt.Printf("part one: infection wins by %v\n", winningTeamsUnitCount)
	}
	immuneWinCount := part2(immuneSystem, infection, 0, 10000)
	fmt.Printf("part two: immune wins by %v\n", immuneWinCount)
}

type stringSlice []string

func (s stringSlice) contains(a string) bool {
	for _, b := range s {
		if a == b {
			return true
		}
	}
	return false
}

type battle struct {
	attacker, defender *group
}

type group struct {
	units      int
	hp         int
	attack     int
	attackType string
	initiative int
	immunities stringSlice
	weaknesses stringSlice
}

func (g *group) boost(boostVal int) *group {
	return &group{
		g.units,
		g.hp,
		g.attack + boostVal,
		g.attackType,
		g.initiative,
		g.immunities,
		g.weaknesses,
	}
}

func (g *group) effectivePower() int {
	return g.units * g.attack
}

func (g *group) findTarget(defenders []*group) *group {
	maxDamage := 0
	var selectedDefenders []*group
	for _, defender := range defenders {
		dmg := g.attackDamage(defender)
		if dmg > maxDamage {
			maxDamage = dmg
			selectedDefenders = []*group{defender}
		} else if maxDamage != 0 && dmg == maxDamage {
			selectedDefenders = append(selectedDefenders, defender)
		}
	}
	if len(selectedDefenders) == 0 {
		return nil
	}
	if len(selectedDefenders) == 1 {
		return selectedDefenders[0]
	}
	maxPower := 0
	var maxDefender *group
	for _, defender := range selectedDefenders {
		power := defender.effectivePower()
		if maxPower < power {
			maxPower = power
			maxDefender = defender
		}
	}
	return maxDefender
}

func (g *group) attackDamage(defender *group) int {
	if defender.immunities.contains(g.attackType) {
		return 0
	}
	if defender.weaknesses.contains(g.attackType) {
		return g.attack * g.units * 2
	}
	return g.attack * g.units
}

func boostAll(src []*group, boost int) []*group {

	copied := make([]*group, len(src))
	for i, g := range src {
		copied[i] = g.boost(boost)
	}
	return copied
}

func selectTargets(attackers, defenders []*group) []battle {
	sort.Slice(attackers, func(i, j int) bool {
		return attackers[i].effectivePower() > attackers[j].effectivePower()
	})
	var targets []battle
	remainingDefenders := make([]*group, len(defenders))
	copy(remainingDefenders, defenders)
	for _, attacker := range attackers {
		defender := attacker.findTarget(remainingDefenders)
		if defender == nil {
			continue
		}

		// remove the taken target from defenders
		for i, d := range remainingDefenders {
			if d == defender {
				remainingDefenders = append(remainingDefenders[:i], remainingDefenders[i+1:]...)
				break
			}
		}

		targets = append(targets, battle{attacker, defender})
	}

	return targets
}

func part1(immuneSystem []*group, infection []*group) (int, bool) {
	prevImmume, prevInfection := 0, 0
	for len(immuneSystem) > 0 && len(infection) > 0 {

		immuneSystemTargets := selectTargets(immuneSystem, infection)
		infectionTargets := selectTargets(infection, immuneSystem)
		allBattles := append(immuneSystemTargets, infectionTargets...)
		sort.Slice(allBattles, func(i, j int) bool {
			return allBattles[i].attacker.initiative > allBattles[j].attacker.initiative
		})

		for _, battle := range allBattles {
			if battle.attacker.units > 0 {
				dmg := battle.attacker.attackDamage(battle.defender)
				unitsLost := dmg / battle.defender.hp
				battle.defender.units -= unitsLost
			}
		}

		immuneSystem = removeDeadGroups(immuneSystem)
		infection = removeDeadGroups(infection)

		if prevImmume == sumUnits(immuneSystem) && prevInfection == sumUnits(infection) {
			fmt.Printf("stalemate. immune: %v. infection: %v\n", sumUnits(immuneSystem), sumUnits(infection))
			return sumUnits(infection), false
		}
		prevImmume, prevInfection = sumUnits(immuneSystem), sumUnits(infection)
	}
	if len(immuneSystem) > 0 {
		return sumUnits(immuneSystem), true
	}
	if len(infection) > 0 {
		return sumUnits(infection), false
	}
	panic("oh dear!")
}

func part2(immuneSystem []*group, infection []*group, startVal, boostVal int) int {

	for boost := startVal; ; boost = boost + boostVal {
		clonedInfection := boostAll(infection, 0)

		boostedImmuneSystem := boostAll(immuneSystem, boost)

		remaining, immuneWins := part1(boostedImmuneSystem, clonedInfection)

		if boostVal == 1 && immuneWins {
			return remaining
		}
		if immuneWins {
			return part2(immuneSystem, infection, boost-boostVal, boostVal/10)
		}
	}

}

func sumUnits(groups []*group) int {
	sum := 0
	for _, g := range groups {
		sum += g.units
	}
	return sum
}

func removeDeadGroups(groups []*group) []*group {
	for i := len(groups) - 1; i > -1; i-- {
		g := groups[i]
		if g.units < 1 {
			groups = append(groups[:i], groups[i+1:]...)
		}
	}
	return groups
}

func parseInput(fileName string) (immuneSystem []*group, infection []*group) {
	f, err := os.Open(fileName)
	if err != nil {
		log.Panicf("could not open file. %v", err)
	}
	defer f.Close()
	atoi := func(x string) int {
		i, _ := strconv.Atoi(x)
		return i
	}
	rx, _ := regexp.Compile(rx)
	weaknessImunitiesRx, _ := regexp.Compile(weaknessImunitiesRx)
	var active *[]*group
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		if scanner.Text() == "" {
			continue
		}
		if scanner.Text() == "Immune System:" {
			active = &immuneSystem
		} else if scanner.Text() == "Infection:" {
			active = &infection
		} else {
			groupStats := rx.FindStringSubmatch(scanner.Text())[1:]
			g := &group{atoi(groupStats[0]), atoi(groupStats[1]), atoi(groupStats[3]), groupStats[4], atoi(groupStats[5]), nil, nil}

			weaknessAndImmunities := weaknessImunitiesRx.FindAllStringSubmatch(groupStats[2], -1)
			if len(weaknessAndImmunities) > 0 {
				for _, x := range weaknessAndImmunities {
					if x[1] == "weak" {
						g.weaknesses = append(g.weaknesses, strings.Split(x[2], ", ")...)
					} else {
						g.immunities = append(g.immunities, strings.Split(x[2], ", ")...)
					}
				}
			}

			*active = append(*active, g)
		}
	}
	return
}
