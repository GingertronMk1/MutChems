"""A player in a position group"""
from src.team.team_or_multiple import TeamOrMultiple


class PositionGroupPlayer:
    """A player in a position group"""

    name: str
    teams: list[TeamOrMultiple]

    def __init__(self, name: str, teams: list[TeamOrMultiple] = None) -> None:
        self.name = name
        if teams is None:
            self.teams = []
        else:
            self.teams = teams

    @staticmethod
    def from_dict(initial_dict: dict) -> "__class__":
        return PositionGroupPlayer(
            name=initial_dict.get("name", "No Name Given"),
            teams=TeamOrMultiple.from_strings(initial_dict.get("teams", [])),
        )
