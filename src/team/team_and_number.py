from src.team.team import Team
from itertools import repeat


class TeamAndNumber:
    team: Team
    number: int = 1

    def __init__(self, team: Team, number: int = 1) -> None:
        self.team = team
        self.number = number

    @staticmethod
    def from_string(init_string: str) -> list["__class__"]:
        return [
            TeamAndNumber.from_string_part(sub_string)
            for sub_string in init_string.split("|")
        ]

    @staticmethod
    def from_string_part(init_string: str) -> "__class__":
        tokens = init_string.split(".")
        team = Team(tokens[0])
        if len(tokens) == 2:
            return TeamAndNumber(team, int(tokens[1]))
        return TeamAndNumber(team)

    def expand(self) -> list[Team]:
        return repeat(self.team, self.number)

    def __str__(self) -> str:
        return f"{self.team.value}.{self.number}"
