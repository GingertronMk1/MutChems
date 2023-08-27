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

        def reduce_function(accumulator: dict, lineup_2: Variation) -> Variation:
            iteration = accumulator.get("iteration", 0) + 1
            current_percent = accumulator.get("current_percent", 0)
            lineup_1 = accumulator.get("lineup")
            total_number_of_lineups = accumulator.get("total_number_of_lineups")
            ret_lineup = lineup_1
            if Value.compare_lineups(lineup_1, lineup_2) < 0:
                ret_lineup = lineup_2
            new_percent = (100 * iteration) // total_number_of_lineups
            if new_percent > current_percent:
                current_percent = new_percent
                print(
                    f"\t{new_percent}% done ({iteration:,} / {total_number_of_lineups:,})"
                )
            accumulator["lineup"] = ret_lineup
            accumulator["iteration"] = iteration
            accumulator["current_percent"] = current_percent
            return accumulator

        bestPossible = functools.reduce(
            reduce_function,
            all_possibles,
            {
                "lineup": all_possibles[0],
                "iteration": 0,
                "current_percent": 0,
                "total_number_of_lineups": total_number_of_lineups,
            },
        ).get("lineup")

        with open(f"outputs/{key}-change.csv", "w", encoding="utf-8") as csvfile:
            bestPossible.write_to_csv(csvfile)
        end_time = time.time()
