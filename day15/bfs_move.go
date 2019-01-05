package main

import (
	"math"
	"sort"
)

type possibleMove struct {
	first     location
	length    int
	targetLoc location
}

type input struct {
	u         string
	l         location
	enemyType unitType
}

type output struct {
	l     location
	moved bool
}

var (
	inputs  []input
	outputs []output
)

func bfsMove(u unitMap, l location, enemyType unitType) (location, bool) {

	inputs = append(inputs, input{u.String(), l, enemyType})
	l, b := _bfsMove(u, l, enemyType)
	outputs = append(outputs, output{l, b})
	return l, b
}

// Perform a breadth first search for each adjacent tile
// Although not the most efficient, the approach is still fast and makes it
// easy to sort in such a way that satisfies all the conditions
func _bfsMove(u unitMap, l location, enemyType unitType) (location, bool) {

	//If an enemy is in range of our current location we are done!
	if u.isInRange(l, enemyType) {
		return l, false
	}

	//Get all positions for the first move
	firstMoves := u.openSquaresInRangeOf(l)

	//Keep the possible moves we've found, in format -
	//(first_move, number_of_moves, tile_coordinates)
	//At the end we'll need to use all these values to find the proper move
	bestMoves := []possibleMove{}

	for _, move := range firstMoves {

		//We might immediately have an adjacent enemy and not need to search further
		if u.isInRange(move, enemyType) {
			bestMoves = append(bestMoves, possibleMove{move, 1, move})
			continue
		}

		//We'll need to keep track of two things -
		//seenCoords - the tiles we've already visited
		//stack - the "new" tiles accessible from the current furthest points
		seenCoords := map[location]struct{}{l: struct{}{}, move: struct{}{}}
		stack := u.openSquaresInRangeOf(move)

		//Now do the search -

		i := 1 //Already have moved one tile at this point
		run := true
		for run {
			i++

			//Keep track of the new tiles here
			newStack := []location{}

			//Loop through and look for new tiles to add
			for _, tile := range stack {
				if _, ok := seenCoords[tile]; ok {
					continue
				}

				seenCoords[tile] = struct{}{}

				if u.isInRange(tile, enemyType) {
					bestMoves = append(bestMoves, possibleMove{move, i, tile})
					//We want to complete this iteration to find all other reachable tiles at the same distance
					run = false
					continue
				}

				//Add all newly accessible tiles to stack
				newStack = append(newStack, u.openSquaresInRangeOf(tile)...)
			}
			stack = make([]location, len(newStack))
			copy(stack, newStack)
			//We might also need to end at this point if we have no more newly accessible tiles
			if len(stack) < 1 {
				run = false
			}
		}
	}
	//Take our list of the bestMoves from each starting point that we generated, and find the one move we'll take
	return getBestMove(bestMoves)
}

// Perform a breadth first search for each adjacent tile
// Although not the most efficient, the approach is still fast and makes it
// easy to sort in such a way that satisfies all the conditions
func __bfsMove(u unitMap, l location, enemyType unitType) (location, bool) {

	//If an enemy is in range of our current location we are done!
	if u.isInRange(l, enemyType) {
		return l, false
	}

	//Get all positions for the first move
	// firstMoves := u.openSquaresInRangeOf(l)

	//Keep the possible moves we've found
	//At the end we'll need to use all these values to find the proper move
	bestMoves := []possibleMove{}

	// for _, move := range firstMoves {

	//We might immediately have an adjacent enemy and not need to search further
	// if u.isInRange(move, enemyType) {
	// 	bestMoves = append(bestMoves, possibleMove{move, 1, move})
	// 	continue
	// }

	//We'll need to keep track of two things -
	//seenCoords - the tiles we've already visited
	//stack - the "new" tiles accessible from the current furthest points
	seenCoords := map[location]struct{}{l: struct{}{}}
	openSquares := u.openSquaresInRangeOf(l)
	stack := make([]possibleMove, len(openSquares))
	for i, s := range openSquares {
		stack[i] = possibleMove{s, 1, s}
	}

	//Now do the search -

	i := 1 //Already have moved one tile at this point
	run := true
	for run {
		i++

		//Keep track of the new tiles here
		newStack := []possibleMove{}

		//Loop through and look for new tiles to add
		for _, tile := range stack {
			if _, ok := seenCoords[tile.targetLoc]; ok {
				continue
			}

			seenCoords[tile.targetLoc] = struct{}{}

			if u.isInRange(tile.targetLoc, enemyType) {
				bestMoves = append(bestMoves, tile)
				//We want to complete this iteration to find all other reachable tiles at the same distance
				run = false
				continue
			}

			//Add all newly accessible tiles to stack
			for _, l := range u.openSquaresInRangeOf(tile.targetLoc) {
				newStack = append(newStack, possibleMove{tile.first, i, l})
			}
		}
		stack = make([]possibleMove, len(newStack))
		copy(stack, newStack)
		//We might also need to end at this point if we have no more newly accessible tiles
		if len(stack) < 1 {
			run = false
		}
	}
	// }
	//Take our list of the bestMoves from each starting point that we generated, and find the one move we'll take
	return getBestMove(bestMoves)
}

// Takes a list of tuples of
// (first_move, number_of_moves, tile_coordinates), which might look like -
// ((12, 22), 8, (17, 25))
// ((12, 22), 8, (18, 24))
// ((12, 22), 8, (19, 21))
// ((13, 21), 6, (19, 21))
// ((13, 23), 6, (17, 25))
// ((13, 23), 6, (18, 24))
// ((14, 22), 6, (17, 25))
// ((14, 22), 6, (18, 24))
// ((14, 22), 6, (19, 21))
// And filters/sorts them to satisfy all the conditions
func getBestMove(moves []possibleMove) (location, bool) {

	if moves == nil || len(moves) < 1 {
		return location{}, false
	}

	//First condition - fewest number of moves away
	minSteps := math.MaxInt64
	for _, n := range moves {
		if n.length < minSteps {
			minSteps = n.length
		}
	}
	bestMoves := make([]possibleMove, 0, 4)
	for _, n := range moves {
		if n.length == minSteps {
			bestMoves = append(bestMoves, n)
		}
	}

	//Second condition - if tie, choose the first tile in reading order
	sort.Slice(bestMoves, func(i, j int) bool {
		return bestMoves[i].targetLoc.Less(bestMoves[j].targetLoc)
	})
	tmp := make([]possibleMove, 0, 4)
	minMove := bestMoves[0].targetLoc
	for _, n := range bestMoves {
		if n.targetLoc == minMove {
			tmp = append(tmp, n)
		}
	}
	bestMoves = tmp

	//Third condition - if tie, take the first step in reading order
	sort.Slice(bestMoves, func(i, j int) bool {
		return bestMoves[i].first.Less(bestMoves[j].first)
	})
	tmp = make([]possibleMove, 0, 4)
	minFirst := bestMoves[0].first
	for _, n := range bestMoves {
		if n.first == minFirst {
			tmp = append(tmp, n)
		}
	}
	bestMoves = tmp

	return bestMoves[0].first, true
}
