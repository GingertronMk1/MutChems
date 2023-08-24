"""A team or multiple teams for chemistries"""
from src.team.team import Team
from src.team.team_and_number import TeamAndNumber


class TeamOrMultiple:
    """A team or multiple teams for chemistries"""
    children: list[TeamAndNumber] = []

    def __init__(self, children: list[TeamAndNumber] = []) -> None:
        self.children = children

    @staticmethod
    def from_string(string: str) -> "__class__":
        return TeamOrMultiple(children=TeamAndNumber.from_string(string))
        

    @staticmethod
    def from_strings(strings: list[str]) -> list["__class__"]:
        return [
            TeamOrMultiple.from_string(string)
            for string in strings
        ]

    def expand(self) -> list[Team]:
        return [
            t
            for child in self.children
            for t in child.expand()
        ]