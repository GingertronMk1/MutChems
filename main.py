import functools
import json
from src.player.current_player import CurrentPlayer
from src.lineup.possible_lineup import PossibleLineup
from src.lineup.position_group import PositionGroup
from src.lineup.value import Value
from src.change.change import Change

with open("./data/team.json", "r", encoding="utf-8") as f:
    data = json.load(f)

print("Loaded data")

allPositionGroups: list[PositionGroup] = [
    PositionGroup(posGroup) for posGroup in data["current"]
]

all_players: list[CurrentPlayer] = [
    player
    for positionGroup in allPositionGroups
    for player in CurrentPlayer.from_position_group(positionGroup)
]

changes: list[Change] = [Change.from_dict(change) for change in data["changes"]]

allLineups: list[list[CurrentPlayer]] = [all_players]

for change in changes:
    allLineups.append(change.apply(allLineups[-1]))

for key, all_players in enumerate(allLineups):
    print("Converted position groups into individual players")

    allPossibles = PossibleLineup.from_regular_lineup(all_players)

    print(f"Generated all possible lineups, total of {len(allPossibles)}")

    bestPossible = max(allPossibles, key=functools.cmp_to_key(Value.compare_lineups))

    print("Determined best possible lineup")

    with open(f"outputs/{key}-change.csv", "w", encoding="utf-8") as csvfile:
        bestPossible.write_to_csv(csvfile)
print("Done!")
