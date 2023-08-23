import functools
import json
from src.player.current_player import CurrentPlayer
from src.lineup.possible_lineup import PossibleLineup
from src.lineup.position_group import PositionGroup
from src.change.change import Change

with open("./data/team.json", "r", encoding="utf-8") as f:
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

    print(F"Generated all possible lineups, total of {len(allPossibles)}")

    bestPossible = max(allPossibles, key=functools.cmp_to_key(PossibleLineup.compare))

    print("Determined best possible lineup")

    with open(F"outputs/{key}-change.csv", "w", encoding="utf-8") as csvfile:
        bestPossible.writeToCsv(csvfile)
print("Done!")
