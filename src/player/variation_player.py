"""One player with a single TeamOrMultiple option"""
from dataclasses import dataclass
from src.team.team_or_multiple import TeamOrMultiple
from src.team.team import Team
from src.player.lineup_player import LineupPlayer
from src.lineup.position import Position


@dataclass
class VariationPlayer:
    """One player with a single TeamOrMultiple option"""

    name: str
    team: TeamOrMultiple
    position: Position

    @staticmethod
    def from_lineup_player(lineup_player: LineupPlayer) -> list["VariationPlayer"]:
        """Generating a list of these from a player with many teams"""
        return [
            VariationPlayer(lineup_player.name, team, lineup_player.position)
            for team in lineup_player.teams
        ]

    def expand_teams(self) -> list[Team]:
        """Get all the teams in the player"""
        return self.team.expand()
