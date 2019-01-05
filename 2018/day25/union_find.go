package main

type unionFind struct {
	n       int
	parents []*int
	ranks   []int
	numSets int
}

func newUnionFind(n int) *unionFind {
	uf := &unionFind{}
	uf.n = n
	uf.parents = make([]*int, n)
	uf.ranks = make([]int, n)
	for i := 0; i < n; i++ {
		uf.ranks[i] = 1
	}
	uf.numSets = n
	return uf
}

func (uf *unionFind) find(i int) int {
	parent := uf.parents[i]
	if parent == nil {
		return i
	}
	p := uf.find(*parent)
	uf.parents[i] = &p
	return p
}

func (uf *unionFind) inSameSet(i, j int) bool {
	return uf.find(i) == uf.find(j)
}

func (uf *unionFind) merge(i, j int) {
	i = uf.find(i)
	j = uf.find(j)

	if i == j {
		return
	}

	iRank := uf.ranks[i]
	jRank := uf.ranks[j]

	if iRank < jRank {
		uf.parents[i] = &j
	} else if iRank > jRank {
		uf.parents[j] = &i
	} else {
		uf.parents[j] = &i
		uf.ranks[i]++
	}
	uf.numSets--
}
