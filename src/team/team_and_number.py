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
        tokens = init_string.split(".")
        # print(tokens[0])
        if tokens[0] == Team.ALL32.value:
            raise Exception("All32 team values shouldn't make it here")

        team = Team(tokens[0])
        if len(tokens) == 2:
            return [TeamAndNumber(team, int(tokens[1]))]
        return [TeamAndNumber(team)]

    def expand(self) -> list[Team]:
        return repeat(self.team, self.number)

    def __str__(self) -> str:
        return f"{self.team.value}.{self.number}"
