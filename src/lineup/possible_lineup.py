from io import TextIOWrapper
from itertools import product, groupby
import csv
from src.player.current_player import CurrentPlayer
from src.player.current_player_single_team import CurrentPlayerSingleTeam


class PossibleLineup:
    THRESHOLD_FULL = 50
    THRESHOLD_HALF = 25

    data: list[CurrentPlayerSingleTeam] = []

    def __init__(self, data: list[CurrentPlayerSingleTeam]) -> None:
        self.data = data

    def __str__(self) -> str:
        return "\n".join([str(player) for player in self.data])

    def all_values(self) -> dict[str, int]:
        for player in self.data:
            print(f"{player.name}, {player.team.expand()}")

        all_toms = [
            team
            for player in self.data
            for team in player.team.expand()
        ]
        individuals = set(all_toms)
        compressed_toms = [
            (t, len(filter(lambda x: x == t, all_toms))) for t in individuals
        ]
        compressed_toms.sort(key=lambda x: x[1])
            
        return dict(compressed_toms)

    def value(self) -> tuple[str, int]:
        val = self.all_values()
        return (max(val, key=val.get), max(val.values()))

    @staticmethod
    def compare(lineup_1: "__class__", lineup_2: "__class__") -> int:
        lineup_1_val = lineup_1.value()
        lineup_2_val = lineup_2.value()
        if lineup_1_val[1] != lineup_2_val[1]:
            return lineup_1_val[1] - lineup_2_val[1]
        return len(lineup_2.all_values()) - len(lineup_1.all_values())

    @staticmethod
    def from_regular_lineup(lineup: list[CurrentPlayer]) -> list["__class__"]:
        all_players = [
            CurrentPlayerSingleTeam.from_current_player(player) for player in lineup
        ]

        return [PossibleLineup(potential) for potential in product(*all_players)]

    def write_to_csv(self, outfile: TextIOWrapper) -> None:
        writer = csv.writer(outfile, delimiter=",")
        writer.writerow(["Name", "Position", "Team"])
        for player in self.data:
            writer.writerow(
                [
                    player.name,
                    player.position.value,
                    " | ".join([str(team) for team in player.team]),
                ]
            )
        writer.writerow([None, None, None])
        best_values = self.all_values()
        for key, value in best_values.items():
            writer.writerow([key.value, value, None])
