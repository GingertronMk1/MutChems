import json
import itertools
import csv
import functools
from src.player.currentPlayer import CurrentPlayer
from src.player.currentPlayerSingleTeam import CurrentPlayerSingleTeam
from src.possibleLineup import PossibleLineup
from src.player.positionGroup import PositionGroup
import sys

with open("./data/team.json", "r") as f:
    data = json.load(f)

print("Loaded data")
print(data)

allPositionGroups = map(PositionGroup, data["current"])

allPlayers = [
    player
    for positionGroup in allPositionGroups
    for player in CurrentPlayer.fromPositionGroup(positionGroup)
]

print("Converted position groups into individual players")

allPotentials = [
    CurrentPlayerSingleTeam.fromCurrentPlayer(player) for player in allPlayers
]

print("Generated single-team players")

allPotentials: list[list[CurrentPlayerSingleTeam]] = itertools.product(*allPotentials)

allPossibles = []
for potential in allPotentials:
    allPossibles.append(PossibleLineup(list(potential)))

print(
    "Generated all possible lineups, total of {total}".format(total=len(allPossibles))
)

bestPossible = max(allPossibles, key=functools.cmp_to_key(PossibleLineup.compare))

print("Determined best possible lineup")

with open("output.csv", "w") as csvfile:
    writer = csv.writer(csvfile, delimiter=",")
    writer.writerow(["Name", "Position", "Team"])
    for player in bestPossible.data:
        # print("Player is a {type}".format(type=type(player)))
        # print("Player team is a {type}".format(type=type(player.team)))
        writer.writerow(
            [
                player.name,
                player.position.value,
                "{team}".format(team=" | ".join(map(str, player.team))),
            ]
        )
    writer.writerow([None, None, None])
    bestValues = bestPossible.allValues()
    for key in bestValues.keys():
        writer.writerow([key.value, bestValues[key]])

print("Done!")
