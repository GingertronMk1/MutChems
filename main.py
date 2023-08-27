"""The main event"""
from copy import deepcopy
import json
import os
import glob
import time
from src.player.lineup_player import LineupPlayer
from src.lineup.value import Value
from src.lineup.lineup import Lineup
from src.change.change import Change

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
        analysis_lineup = deepcopy(working_lineup)
        start_time = time.time()
        best_possible = Value.get_best_lineup_variation(analysis_lineup)

        with open(f"outputs/{key}-change.csv", "w", encoding="utf-8") as csvfile:
            best_possible.write_to_csv(csvfile)
        end_time = time.time()
