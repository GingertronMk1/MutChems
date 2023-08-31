"""The main event"""
from copy import deepcopy
import json
import os
import glob
import time
import sys
from src.variation.value import Value
from src.lineup.lineup import Lineup
from src.change.change import Change
from src.lineup.position_group import PositionGroup
from io import TextIOWrapper

if __name__ == "__main__":
    old_csvs = glob.glob("outputs/*.csv")
    for old_csv in old_csvs:
        os.remove(old_csv)

    f: TextIOWrapper = open("./input.json", "r", encoding="utf-8")
    data = json.load(f)

    posGroups = [PositionGroup.from_dict(d) for d in data.get("current", [])]

    working_lineup: Lineup = Lineup.from_position_groups(posGroups)

    changes: list[Change] = [
        Change.from_dict(change) for change in data.get("changes", [])
    ]
    changes.insert(0, Change())

    if len(sys.argv) > 1 and sys.argv[1] == "print_changes":
        print("Just printing changes")
        for change in changes:
            print(change.pretty_print())
        sys.exit(0)

    for key, change in enumerate(changes):
        change_string = ""
        file_name = ""
        match (key):
            case 0:
                change_string = "Current lineup with no changes"
                file_name = "0_current_lineup.csv"
            case 1:
                change_string = f"1 change: `{change.pretty_print()}`"
                file_name = "1_change.csv"
            case n:
                change_string = f"{n} changes: `{change.pretty_print()}"
                file_name = f"{n}_changes.csv"
        print(change_string)
        working_lineup = change.apply(working_lineup)
        analysis_lineup = deepcopy(working_lineup)
        start_time = time.time()
        best_possible = Value.get_best_lineup_variation(analysis_lineup)

        with open(f"outputs/{file_name}", "w", encoding="utf-8") as csvfile:
            best_possible.write_to_csv(csvfile)
        end_time = time.time()
