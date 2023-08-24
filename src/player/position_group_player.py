"""A player in a position group"""
from src.team.team_or_multiple import TeamOrMultiple
from src.team.team import Team


class PositionGroupPlayer:
    """A player in a position group"""

    name: str
    teams: list[TeamOrMultiple]

    def __init__(self, initial_dict: dict) -> None:
        self.name = initial_dict["name"]
        self.teams = TeamOrMultiple.from_strings(initial_dict["teams"])
