"""A team or multiple teams for chemistries"""
from src.team.team import Team
from src.team.team_and_number import TeamAndNumber
from itertools import takewhile


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
        return_val = []
        for string in strings:
            taken = "".join(takewhile(lambda x: x != ".", string))
            if taken == Team.ALL32.value:
                return_val.extend(
                    [
                        TeamOrMultiple.from_string(t.value)
                        for t in list(Team)
                        if t != Team.ALL32
                    ]
                )
            else:
                return_val.append(TeamOrMultiple.from_string(string))
        return return_val

    def expand(self) -> list[Team]:
        # print([type(t) for t in self.children])
        return [t for child in self.children for t in child.expand()]
