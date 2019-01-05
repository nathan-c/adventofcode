package main

import (
	"fmt"
)

func main() {
	bots := readInput("input.txt")
	botsInRange := part1(bots)
	fmt.Printf("part one: %v bots in range\n", botsInRange)
	// part one: 408 bots in range
	dist, count := part2(bots)
	fmt.Printf("part two: %v far from origin in range of %v bots\n", dist, count)
	// part two: 121167568 far from origin in range of 977 bots
}

type bounds struct {
	min, max int
}

func part1(bots []nanoBot) int {
	var maxRad nanoBot
	for _, x := range bots {
		if x.r > maxRad.r {
			maxRad = x
		}
	}
	count := 0
	for _, x := range bots {
		if maxRad.isInRange(x.location) {
			count++
		}
	}
	return count
}

func part2(bots []nanoBot) (int, int) {

	// Find the range of the bots
	var minx, maxx, miny, maxy, minz, maxz int
	for _, b := range bots {
		minx = min(b.x, minx)
		miny = min(b.y, miny)
		minz = min(b.z, minz)
		maxx = max(b.x, maxx)
		maxy = max(b.x, maxy)
		maxz = max(b.x, maxz)
	}
	xs := bounds{minx, maxx}
	ys := bounds{miny, maxy}
	zs := bounds{minz, maxz}

	// Pick a starting resolution big enough to find all of the bots
	dist := 1
	for dist < xs.max-xs.min || dist < ys.max-ys.min || dist < zs.max-zs.min {
		dist *= 2
	}

	// Try to find all of the bots, backing off with a binary search till
	// we can find the most bots
	// span is a value we use to alter forced count
	// it should first be greater than the length of bots
	// it will then be halved whenever we find no values for a given forcedCount
	// until it reaches 1 at which point we should have hit the end
	span := 1
	for span < len(bots) {
		span *= 2
	}
	// forcedCount is used to restrict the possible points looked at.
	// 1 means the first run with allow any number of bots greater than one to satisfy the search
	// if we find a value that satisfied the forcedCount we increate forcedCount by span
	// if we dont find any matching values for a given forcedCount (i.e. there are less bots to than forcedCount) then it is reduced by span
	forcedCount := 1

	// We might try the same value of forcedCount multiple times, save some time if we've seen it already by caching it
	type values struct {
		distance, count int
		ok              bool
	}
	cache := make(map[int]values)

	var best values

	for {
		vals, ok := cache[forcedCount]
		if !ok {
			dist, count, ok := find(bots, xs, ys, zs, dist, forcedCount)
			vals = values{dist, count, ok}
			cache[forcedCount] = vals
		}

		if !vals.ok {
			// Nothing found for this forcedCount, so half span and reduce forcedCount by the new span
			if span > 1 {
				span = span / 2
			}
			forcedCount = max(1, forcedCount-span)
		} else {
			// We found something (it may not be the best overall value but its valid for the number of bots we asked find to produce)
			// Now we need to up the number we want to find and see if we can do better
			if !best.ok || vals.count > best.count {
				best = vals
			}
			if span == 1 {
				// This means we went back one, and it was empty, so we're done!
				break
			}
			// increase our forcedCount
			forcedCount += span
		}
	}

	return best.distance, best.count
}

func find(bots []nanoBot, xs, ys, zs bounds, dist, forcedCount int) (int, int, bool) {

	type t struct {
		x, y, z, count, dist int
	}

	atTarget := make([]t, 0, 8)

	// search the 8 corners of a cube (or if 1st time potentially just 1 corner (0,0))
	// from each corner count the number of bots that are "within range" of a box surrounding the current point with radius `dist`
	for x := xs.min; x < xs.max+1; x = x + dist {
		for y := ys.min; y < ys.max+1; y = y + dist {
			for z := zs.min; z < zs.max+1; z = z + dist {

				// See how many bots are possible
				count := 0
				for _, b := range bots {
					calc := b.dist(location{x, y, z})
					if dist == 1 {
						if calc <= b.r {
							count++
						}
					} else {
						// The minus three is to include the current box
						// in any bots that are near it.
						// if the calculated distance `calc` is less far from the current point than 3*dist
						// then it falls within a box surrounding the current point with radius `dist`
						if calc-3*dist <= (b.r) {
							count++
						}
					}
				}
				// we accept any number of points greater than forcedCount. this method gets called with varying forcedCounts to probe different paths
				if count >= forcedCount {
					atTarget = append(atTarget, t{x, y, z, count, abs(x) + abs(y) + abs(z)})
				}
			}
		}
	}
	for len(atTarget) > 0 {
		var best t
		bestI := -1

		// Find the closest candidate from the possible boxes
		for i := range atTarget {
			if bestI == -1 || atTarget[i].dist < best.dist {
				best = atTarget[i]
				bestI = i
			}
		}

		if dist == 1 {
			// At the end, just return the best match
			return best.dist, best.count, true
		}
		// Search in the sub boxes, see if we find any matches
		xs = bounds{best.x, best.x + dist/2}
		ys = bounds{best.y, best.y + dist/2}
		zs = bounds{best.z, best.z + dist/2}
		a, b, ok := find(bots, xs, ys, zs, dist/2, forcedCount)
		if !ok {
			// This is a false path, remove it from consideration and try any others
			atTarget = append(atTarget[:bestI], atTarget[bestI+1:]...)
		} else {
			// We found something, go ahead and let it bubble up
			return a, b, true
		}
	}

	// This means all of the candidates yeild false paths, so let this one
	// be treated as a false path by our caller
	return -1, -1, false
}
func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func (l location) dist(x location) int {
	return abs(l.x-x.x) + abs(l.y-x.y) + abs(l.z-x.z)
}

func min(a, b int) int {
	if a > b {
		return b
	}
	return a
}

func max(a, b int) int {
	if a < b {
		return b
	}
	return a
}
