from src.teamOrMultiple import TeamOrMultiple
from src.team import Team


class PositionGroupPlayer:
    name: str
    teams: list[TeamOrMultiple]

    def __init__(self, dict: dict) -> None:
        self.name = dict["name"]
        if dict["teams"] == [Team.ALL32.value]:
            self.teams = [[TeamOrMultiple(t)] for t in Team if t != Team.ALL32]
        else:
            self.teams = [TeamOrMultiple.fromString(team) for team in dict["teams"]]
