import functools
import itertools
import json
import os
import glob
import time
import sys
from src.player.current_player import CurrentPlayer
from src.lineup.variation import Variation
from src.lineup.value import Value
from src.lineup.lineup import Lineup
from src.change.change import Change
from src.change.change_type import ChangeType
from src.team.team import Team


def clear_screen() -> None:
    match os.name:
        case "nt":
            cmd = "cls"
        case other:
            cmd = "clear"
    _ = os.system(cmd)


if __name__ == "__main__":
    files = glob.glob("outputs/*.csv")
    for f in files:
        os.remove(f)

    with open("./data/team.json", "r", encoding="utf-8") as f:
        data = json.load(f)

   # print("Loaded data")

    all_players: Lineup = Lineup(
        player
        for posGroup in data["current"]
        for player in CurrentPlayer.from_position_group_dict(posGroup)
    )

    all_changes_dicts = data["changes"]

    changes: list[Change] = [Change.from_dict(change) for change in data["changes"]]
    changes = [Change(ChangeType.NOCHANGE)] + changes

    for key, change in enumerate(changes):
        all_players = change.apply(all_players)
        print(all_players.pretty_print())
        all_players = all_players.filter_to_n_options()
        start_time = time.time()

        total_number_of_lineups = all_players.num_options()

        all_possibles = all_players.to_variations()

        n: int = 0
        current_percent: int = 0

        def reduce_function(lineup_1: Variation, lineup_2: Variation) -> Variation:
            global n
            global current_percent
            ret_lineup = lineup_1
            if Value.compare_lineups(lineup_1, lineup_2) < 0:
                ret_lineup = lineup_2
            n += 1
            new_percent = int((n / total_number_of_lineups) * 100)
            if new_percent > current_percent:
                current_percent = new_percent
                print(f"\t{new_percent}% done...")
            return ret_lineup

        bestPossible = functools.reduce(
            reduce_function,
            all_possibles,
        )  # max(all_possibles, key=functools.cmp_to_key(Value.compare_lineups))

       # print("\tDetermined best possible lineup")

        with open(f"outputs/{key}-change.csv", "w", encoding="utf-8") as csvfile:
            bestPossible.write_to_csv(csvfile)
        end_time = time.time()
       # print(f"\tCompleted change {key} in {(end_time - start_time):.2f} seconds")
   # print("Done!")
