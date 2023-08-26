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

    all_players: list[CurrentPlayer] = [
        player
        for posGroup in data["current"]
        for player in CurrentPlayer.from_position_group_dict(posGroup)
    ]

    working_lineup: Lineup = Lineup(all_players)

    changes: list[Change] = [
        Change.from_dict(change) for change in data.get("changes", [])
    ]
    changes.insert(0, Change())

    for key, change in enumerate(changes):
        print(f"Applying change {key}: `{change.pretty_print()}`")
        working_lineup = change.apply(working_lineup)

        lineup = working_lineup.filter_to_n_options()
        start_time = time.time()
        total_number_of_lineups = lineup.num_options()
        all_possibles = lineup.to_variations()

        print(f"Checking {total_number_of_lineups:,} options")

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
                print(f"\t{new_percent}% done ({n:,} / {total_number_of_lineups:,})")
            return ret_lineup

        bestPossible = functools.reduce(
            reduce_function,
            all_possibles,
        )

        with open(f"outputs/{key}-change.csv", "w", encoding="utf-8") as csvfile:
            bestPossible.write_to_csv(csvfile)
        end_time = time.time()
