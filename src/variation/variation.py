"""One option for a given Lineup"""
from dataclasses import dataclass, field
from io import TextIOWrapper
import csv
from src.player.variation_player import VariationPlayer
from src.team.team import Team


@dataclass
class Variation:
    """Basically an option for a given lineup - one set of team chemistry assignments"""

    THRESHOLD_FULL = 50
    THRESHOLD_HALF = 25

    players: list[VariationPlayer] = field(default_factory=list)

    def __str__(self) -> str:
        """To string"""
        return "\n".join([str(player) for player in self.players])

    def all_teams(self) -> list[Team]:
        """Get all teams in variation"""
        return [team for player in self.players for team in player.expand_teams()]

    def all_values(self) -> dict[Team, int]:
        """Get all teams and how many are represented"""
        all_toms = self.all_teams()
        individuals = set(all_toms)
        compressed_toms = [
            (t, len([x for x in all_toms if x == t])) for t in individuals
        ]
        compressed_toms.sort(key=lambda x: x[1], reverse=True)

        return dict((key, value) for (key, value) in compressed_toms)

    def value(self) -> tuple[Team, int]:
        """Get a tuple containing the maximum team"""
        val = self.all_values()
        return (max(val, key=val.__getitem__), max(val.values()))

    def write_to_csv(self, outfile: TextIOWrapper) -> None:
        """Write the best option to a given CSV file"""
        writer = csv.writer(outfile, delimiter=",")
        writer.writerow(["Name", "Position", "Team"])
        player_position = None
        for player in self.players:
            if player_position is None:
                player_position = player.position
            elif player_position != player.position:
                writer.writerow(["---", "---", "---"])
                player_position = player.position
            writer.writerow([player.name, player.position.value.name, player.team])
        writer.writerow([None, None, None])
        writer.writerow(["TOTALS", None, None])
        best_values = self.all_values()
        for team, value in best_values.items():
            all_players_in_team = [
                player for player in self.players if team in player.expand_teams()
            ]
            writer.writerow([team.value, value, None])
            for player in all_players_in_team:
                writer.writerow([f"    - {player.name}", None, None])

    def contains_no_team_players(self) -> bool:
        """Does the variation contain players with NoTeam"""
        return Team.NO_TEAM in self.all_teams()
