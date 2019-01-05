package main

import (
	"math"
	"sort"
)

type n struct {
	first     location
	length    int
	targetLoc location
}

// Perform a breadth first search for each adjacent tile
// Although not the most efficient, the approach is still fast and makes it
// easy to sort in such a way that satisfies all the conditions
func bfs_move(u unitMap, l location, enemyType unitType) (location, bool) {

	//If an enemy is located adjacent to our current location - no move!
	if u.isInRange(l, enemyType) {
		return l, false
	}

	//Filter down to valid first moves - must be a '.' there
	first_moves := u.openSquaresInRangeOf(l)

	//Keep the list of tuples nearest tiles we've found, in format -
	//(first_move, number_of_moves, tile_coordinates)
	//At the end we'll need to use all these values to find the proper move
	best_moves := []n{}

	for _, move := range first_moves {

		//We might immediately have an adjacent enemy and not need to search further
		if u.isInRange(move, enemyType) {
			best_moves = append(best_moves, n{move, 1, move})
			continue
		}

		//We'll need to keep track of two things -
		//seen_coordinates - the tiles we've already visited
		//stack - the "new" tiles accessible from the current furthest points
		seen_coordinates := map[location]struct{}{l: struct{}{}, move: struct{}{}}
		stack := u.openSquaresInRangeOf(move)

		//Now do the search -

		i := 1 //Already have moved one tile at this point
		run := true
		for run {
			i += 1

			//Keep track of the new tiles here
			new_stack := []location{}

			//Loop through and look for new tiles to add
			for _, tile := range stack {
				if _, ok := seen_coordinates[tile]; ok {
					continue
				}

				seen_coordinates[tile] = struct{}{}

				if u.isInRange(tile, enemyType) {
					best_moves = append(best_moves, n{move, i, tile})
					//We want to complete this iteration to find all other reachable tiles at the same distance
					run = false
					continue
				}

				//Add all newly accessible tiles to stack
				new_stack = append(new_stack, u.openSquaresInRangeOf(tile)...)
			}
			stack = make([]location, len(new_stack))
			copy(stack, new_stack)
			//We might also need to end at this point if we have no more newly accessible tiles
			if len(stack) < 1 {
				run = false
			}
		}
	}
	//Take our list of the best_moves from each starting point that we generated, and find the one move we'll take
	return get_best_move(best_moves)
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
func get_best_move(moves []n) (location, bool) {

	if moves == nil || len(moves) < 1 {
		return location{}, false
	}

	//First condition - fewest number of moves away
	min_steps := math.MaxInt64
	for _, n := range moves {
		if n.length < min_steps {
			min_steps = n.length
		}
	}
	best_moves := make([]n, 0, 4)
	for _, n := range moves {
		if n.length == min_steps {
			best_moves = append(best_moves, n)
		}
	}

	//Second condition - if tie, choose the first tile in reading order
	sort.Slice(best_moves, func(i, j int) bool {
		return best_moves[i].targetLoc.Less(best_moves[j].targetLoc)
	})
	tmp := make([]n, 0, 4)
	minMove := best_moves[0].targetLoc
	for _, n := range best_moves {
		if n.targetLoc == minMove {
			tmp = append(tmp, n)
		}
	}
	best_moves = tmp

	//Third condition - if tie, take the first step in reading order
	sort.Slice(best_moves, func(i, j int) bool {
		return best_moves[i].first.Less(best_moves[j].first)
	})
	tmp = make([]n, 0, 4)
	minFirst := best_moves[0].first
	for _, n := range best_moves {
		if n.first == minFirst {
			tmp = append(tmp, n)
		}
	}
	best_moves = tmp

	return best_moves[0].first, true
}
