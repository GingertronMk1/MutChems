import functools
import json
import os
import glob
import time
import sys
from src.player.lineup_player import LineupPlayer
from src.lineup.variation import Variation
from src.lineup.value import Value
from src.lineup.lineup import Lineup
from src.change.change import Change


def clear_screen() -> None:
    match os.name:
        case "nt":
            cmd = "cls"
        case _:
            cmd = "clear"
    _ = os.system(cmd)


if __name__ == "__main__":
    files = glob.glob("outputs/*.csv")
    for f in files:
        os.remove(f)

    with open("./input.json", "r", encoding="utf-8") as f:
        data = json.load(f)

    all_players: list[LineupPlayer] = [
        player
        for posGroup in data["current"]
        for player in LineupPlayer.from_position_group_dict(posGroup)
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

        best_possible = Value.find_best_possible_variation(all_possibles)

        with open(f"outputs/{key}-change.csv", "w", encoding="utf-8") as csvfile:
            best_possible.write_to_csv(csvfile)
        end_time = time.time()
