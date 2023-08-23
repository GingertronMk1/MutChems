"""A player in a position group"""
from src.team_or_multiple import TeamOrMultiple
from src.team import Team


class PositionGroupPlayer:
    """A player in a position group"""

    name: str
    teams: list[TeamOrMultiple]

    def __init__(self, initial_dict: dict) -> None:
        self.name = initial_dict["name"]
        if initial_dict["teams"] == [Team.ALL32.value]:
            self.teams = [[TeamOrMultiple(t)] for t in Team if t != Team.ALL32]
        else:
            self.teams = [
                TeamOrMultiple.from_string(team) for team in initial_dict["teams"]
            ]
