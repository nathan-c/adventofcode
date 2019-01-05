import day13


def test_attempt_firewall():
    assert day13.attempt_firewall({0: 3, 1: 2, 4: 4, 6: 4}) == 24


def test_try_attempt_firewall():
    assert day13.try_attempt_firewall({0: 3, 1: 2, 4: 4, 6: 4}) == 10


def test_try_attempt_firewall_fast():
    assert day13.try_attempt_firewall_fast({0: 3, 1: 2, 4: 4, 6: 4}) == 10
