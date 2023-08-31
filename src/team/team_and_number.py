"""Simple team/number combo - could probably be a named tuple tbh"""
from dataclasses import dataclass
from itertools import repeat
from src.team.team import Team


@dataclass
class TeamAndNumber:
    """Simple team/number combo - could probably be a named tuple tbh"""

    team: Team
    number: int = 1

    @staticmethod
    def from_string(init_string: str) -> list["TeamAndNumber"]:
        """Generate from a string"""
        return [
            TeamAndNumber.from_string_part(sub_string)
            for sub_string in init_string.split("|")
        ]

    @staticmethod
    def from_string_part(init_string: str) -> "TeamAndNumber":
        """Generate from a part of a string"""
        tokens = init_string.split(".")
        team = Team(tokens[0])
        if len(tokens) == 2:
            return TeamAndNumber(team, int(tokens[1]))
        return TeamAndNumber(team)

    def expand(self) -> list[Team]:
        """Expand into a list of Teams for counting purposes"""
        return [self.team] * self.number

    def __str__(self) -> str:
        """Re-encode"""
        return f"{self.team.value}.{self.number}"
