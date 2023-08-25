import functools
import itertools
import json
import os
import glob
import time
from src.player.current_player import CurrentPlayer
from src.lineup.possible_lineup import PossibleLineup
from src.lineup.position_group import PositionGroup
from src.lineup.value import Value
from src.change.change import Change
from src.change.change_type import ChangeType

if __name__ == "__main__":
    files = glob.glob("outputs/*.csv")
    for f in files:
        os.remove(f)

    with open("./data/team.json", "r", encoding="utf-8") as f:
        data = json.load(f)

    print("Loaded data")

    all_players = [
        player
        for posGroup in data["current"]
        for player in CurrentPlayer.from_position_group_dict(posGroup)
    ]

    all_changes_dicts = data["changes"]

    data["changes"].insert(0, {"type": ChangeType.NOCHANGE.value})

    changes: list[Change] = [Change.from_dict(change) for change in data["changes"]]

    for key, change in enumerate(changes):
        print(change.pretty_print())
        all_players = change.apply(all_players)
        print("\t New lineup:")
        for player in all_players:
            print(f"\t\t{str(player)}")
        start_time = time.time()
        print("\tConverting position groups into individual players")

        all_possibles = PossibleLineup.from_regular_lineup(all_players)

        print(f"\tGenerated all possible lineups, total of {len(all_possibles)}")

        bestPossible = functools.reduce(
            lambda l1, l2: l1 if Value.compare_lineups(l1, l2) > 0 else l2,
            all_possibles,
        )  # max(all_possibles, key=functools.cmp_to_key(Value.compare_lineups))

        print("\tDetermined best possible lineup")

        with open(f"outputs/{key}-change.csv", "w", encoding="utf-8") as csvfile:
            bestPossible.write_to_csv(csvfile)
        end_time = time.time()
        print(f"\tCompleted change {key} in {(end_time - start_time):.2f} seconds")
    print("Done!")
