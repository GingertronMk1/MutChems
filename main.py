import json
import itertools
import csv
from src.currentPlayer import CurrentPlayer
from src.currentPlayerSingleTeam import CurrentPlayerSingleTeam
from src.possibleLineup import PossibleLineup

with open("./data/team.json", "r") as f:
    data = json.load(f)

allPlayers = []

for position in data["current"]:
    for player in position["players"]:
        allPlayers.append(CurrentPlayer(player, position["position"]))

allPotentials = []

for player in allPlayers:
    allPotentials.append(CurrentPlayerSingleTeam.fromCurrentPlayer(player))

allPotentials: list[list[CurrentPlayerSingleTeam]] = itertools.product(*allPotentials)

allPossibles = []
for potential in allPotentials:
    allPossibles.append(PossibleLineup(list(potential)))

allPossibles: list[PossibleLineup] = PossibleLineup.sort(allPossibles)

bestPossible = allPossibles[-1]

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
