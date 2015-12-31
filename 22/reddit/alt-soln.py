#!/usr/bin/python2.7
# Copyright © 2015 Bart Massey
# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.

# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.
# https://www.reddit.com/r/adventofcode/comments/
#   3xspyl/day_22_solutions/cy7l25a

def sim(actions, part):
    boss_hp, boss_dmg = 58, 9
    hp, mana, armor = 50, 500, 0
    turn, turn_c = 0, 0
    mana_spent = 0
    poison_left, shield_left, recharge_left = 0, 0, 0
    my_turn = 1
    spell_cost = {'M': 53, 'D': 73, 'S': 113, 'P': 173, 'R': 229}

    while True:
        if len(actions)-1 < turn_c:
            print 'out of moves'
            return 0
        if poison_left:
            poison_left = max(poison_left - 1, 0)
            boss_hp -= 3
        if shield_left:
            shield_left = max(shield_left - 1, 0)
            armor = 7
        else:
            armor = 0        
        if recharge_left:
            recharge_left = max(recharge_left - 1, 0)
            mana += 101
        if my_turn == 1:
            if part == 2:
                hp -= 1
                if hp <= 0:
                    return 0
            action = actions[turn_c]
            mana -= spell_cost[action]
            mana_spent += spell_cost[action]
            if action == 'M':
                boss_hp -= 4
            elif action == 'D':
                boss_hp -= 2
                hp += 2
            elif action == 'S':
                if shield_left:
                    return 0
                shield_left = 6
            elif action == 'P':
                if poison_left:
                    return 0
                poison_left = 6
            elif action == 'R':
                if recharge_left:
                    return 0
                recharge_left = 5
            if mana < 0:
                return 0
        if boss_hp <= 0:
            return mana_spent
        if my_turn == -1:
            hp -= max(boss_dmg - armor, 1)
            if hp <= 0:
                return 0
        if my_turn == 1:
            turn_c += 1
        my_turn = -my_turn
        turn += 1

def iterate_actions(pos):
    actions[pos] = 'DSPRM'['MDSPR'.index(actions[pos])]
    if actions[pos] == 'M':
        if pos+1 <= len(actions):
            iterate_actions(pos+1)

for part in (1, 2):
    actions = ['M'] * 20
    min_spent = 1000000
    for i in range(1000000):
        result = sim(actions, part)
        if result:
            min_spent = min(result, min_spent)
        iterate_actions(0)    
    actions = ['M'] * 20
    for i in range(1000000):
        result = sim(actions, part)
        if result:
            print min_spent, actions
            break
        iterate_actions(0)    
