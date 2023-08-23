import json
import itertools
import csv
import functools
from src.player.currentPlayer import CurrentPlayer
from src.player.currentPlayerSingleTeam import CurrentPlayerSingleTeam
from src.lineup.possibleLineup import PossibleLineup
from src.lineup.positionGroup import PositionGroup
from src.change.change import Change
import sys

with open("./data/team.json", "r") as f:
    data = json.load(f)

print("Loaded data")

allPositionGroups: list[PositionGroup] = [PositionGroup(posGroup) for posGroup in data["current"]]

allPlayers: list[CurrentPlayer] = [
    player
    for positionGroup in allPositionGroups
    for player in CurrentPlayer.fromPositionGroup(positionGroup)
]

changes: list[Change] = [
    Change.fromDict(change) for change in data["changes"]
]

allLineups: list[list[CurrentPlayer]] = [allPlayers]

for change in changes:
    allLineups.append(change.apply(allLineups[-1]))

for lineup in allLineups:
  for player in lineup:
    print(player)

for key, allPlayers in enumerate(allLineups):
  print("Converted position groups into individual players")

  allPotentials = [
      CurrentPlayerSingleTeam.fromCurrentPlayer(player) for player in allPlayers
  ]

  print("Generated single-team players")

  allPotentials: list[list[CurrentPlayerSingleTeam]] = itertools.product(*allPotentials)

  allPossibles: list[PossibleLineup] = [PossibleLineup(list(potential)) for potential in allPotentials]

  print(
      "Generated all possible lineups, total of {total}".format(total=len(allPossibles))
  )

  bestPossible = max(allPossibles, key=functools.cmp_to_key(PossibleLineup.compare))

  print("Determined best possible lineup")

  with open("outputs/{n}-{change}.csv".format(n=key, change="change" if key == 1 else "changes"), "w") as csvfile:
      writer = csv.writer(csvfile, delimiter=",")
      writer.writerow(["Name", "Position", "Team"])
      for player in bestPossible.data:
          # print("Player is a {type}".format(type=type(player)))
          # print("Player team is a {type}".format(type=type(player.team)))
          writer.writerow(
              [
                  player.name,
                  player.position.value,
                  "{team}".format(team=" | ".join([str(team) for team in player.team])),
              ]
          )
      writer.writerow([None, None, None])
      bestValues = bestPossible.allValues()
      for key in bestValues.keys():
          writer.writerow([key.value, bestValues[key]])

print("Done!")
