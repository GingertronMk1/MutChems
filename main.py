import json
from src.player.currentPlayer import CurrentPlayer
from src.lineup.possibleLineup import PossibleLineup
from src.lineup.positionGroup import PositionGroup
from src.change.change import Change
import functools

with open("./data/team.json", "r") as f:
    data = json.load(f)

print("Loaded data")

allPositionGroups: list[PositionGroup] = [
    PositionGroup(posGroup) for posGroup in data["current"]
]

allPlayers: list[CurrentPlayer] = [
    player
    for positionGroup in allPositionGroups
    for player in CurrentPlayer.fromPositionGroup(positionGroup)
]

changes: list[Change] = [Change.fromDict(change) for change in data["changes"]]

allLineups: list[list[CurrentPlayer]] = [allPlayers]

for change in changes:
    allLineups.append(change.apply(allLineups[-1]))

for key, allPlayers in enumerate(allLineups):
    print("Converted position groups into individual players")

    allPossibles = PossibleLineup.fromRegularLineup(allPlayers)

    print(
        "Generated all possible lineups, total of {total}".format(
            total=len(allPossibles)
        )
    )

    bestPossible = max(allPossibles, key=functools.cmp_to_key(PossibleLineup.compare))

    print("Determined best possible lineup")

    with open("outputs/{n}-change.csv".format(n=key), "w") as csvfile:
        bestPossible.writeToCsv(csvfile)
print("Done!")
