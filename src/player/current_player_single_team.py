"""One player with a single TeamOrMultiple option"""
from src.team_or_multiple import TeamOrMultiple
from src.player.current_player import CurrentPlayer
from src.position import Position


class CurrentPlayerSingleTeam:
    """One player with a single TeamOrMultiple option"""

    name: str = ""
    team: TeamOrMultiple
    position: Position

    def __init__(self, name: str, team: TeamOrMultiple, position: Position) -> None:
        self.name = name
        self.team = team
        self.position = position

    @staticmethod
    def from_current_player(current_player: CurrentPlayer) -> list["__class__"]:
        """Generating a list of these from a player with many teams"""
        return [
            CurrentPlayerSingleTeam(current_player.name, team, current_player.position)
            for team in current_player.teams
        ]

    def __repr__(self) -> str:
        return f"{self.name} | {self.team} | {self.position}"
