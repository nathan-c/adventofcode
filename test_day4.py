import day4

def test_is_valid_false():
    assert day4.is_valid(["hello", "oelhl"]) == False

def test_is_valid_true():
    assert day4.is_valid(["hello", "oelh"]) == True

def test_is_valid_true2():
    assert day4.is_valid(["hello", "oelha"]) == True