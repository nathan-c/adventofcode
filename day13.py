def attempt_firewall(fire_wall):
    severity = 0
    max_depth = max(fire_wall.keys())
    for i in range(max_depth + 1):
        if i in fire_wall and i % (fire_wall[i] * 2 - 2) == 0:
            severity += i * fire_wall[i]
    return severity
