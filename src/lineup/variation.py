"""One option for a given Lineup"""
from io import TextIOWrapper
import csv
from src.player.current_player_single_team import CurrentPlayerSingleTeam


class Variation:
    """Basically an option for a given lineup - one set of team chemistry assignments"""

    THRESHOLD_FULL = 50
    THRESHOLD_HALF = 25

    data: list[CurrentPlayerSingleTeam] = []

    def __init__(self, data: list[CurrentPlayerSingleTeam]) -> None:
        self.data = data

    def __str__(self) -> str:
        """To string"""
        return "\n".join([str(player) for player in self.data])

    def all_values(self) -> dict[str, int]:
        """Get all teams and how many are represented"""
        all_toms = [team for player in self.data for team in player.team.expand()]
        individuals = set(all_toms)
        compressed_toms = [
            (t, len([x for x in all_toms if x == t])) for t in individuals
        ]
        compressed_toms.sort(key=lambda x: x[1], reverse=True)

        return dict(compressed_toms)

    def value(self) -> tuple[str, int]:
        """Get a tuple containing the maximum team"""
        val = self.all_values()
        return (max(val, key=val.get), max(val.values()))

    def write_to_csv(self, outfile: TextIOWrapper) -> None:
        """Write the best option to a given CSV file"""
        writer = csv.writer(outfile, delimiter=",")
        writer.writerow(["Name", "Position", "Team"])
        player_position = None
        for player in self.data:
            if player_position is None:
                player_position = player.position
            elif player_position != player.position:
                writer.writerow(["---", "---", "---"])
                player_position = player.position
            writer.writerow(
                [
                    player.name,
                    player.position.value,
                    " | ".join([str(team) for team in player.team.children]),
                ]
            )
        writer.writerow([None, None, None])
        writer.writerow(["TOTALS", None, None])
        best_values = self.all_values()
        for key, value in best_values.items():
            writer.writerow([key.value, value, None])
