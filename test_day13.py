import day13


def test_attempty_firewall():
    assert day13.attempt_firewall({0: 3, 1: 2, 4: 4, 6: 4}) == 24
