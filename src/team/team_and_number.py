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
        if tokens[0] == Team.ALL32.value:
            n = 1
            if len(tokens) == 2:
                n = int(tokens[1])
            return [TeamAndNumber(t, n) for t in list(Team) if t != Team.ALL32]
        if len(tokens) == 2:
            return [TeamAndNumber(tokens[0], int(tokens[1]))]
        return [TeamAndNumber(tokens[0])]

    def expand(self) -> list[Team]:
        return repeat(self.team, self.number)