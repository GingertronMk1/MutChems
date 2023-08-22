from src.teamOrMultiple import TeamOrMultiple
from src.position import Position
from src.team import Team


class CurrentPlayer:
    name: str = ""
    teams: list[TeamOrMultiple] = []
    position: Position

    def __init__(self, dict: dict, position: str) -> None:
        self.name = dict["name"]
        if dict["teams"] == [Team.ALL32.value]:
          self.teams = [[TeamOrMultiple(t)] for t in Team if t != Team.ALL32]
        else:
            self.teams = map(TeamOrMultiple.fromString, dict["teams"])
        self.position = Position(position)

    def __str__(self) -> str:
        txt = "Player: {name}\n  Position: {position}\n  Teams: {teams}"
        return txt.format(
            name=self.name, position=self.position, teams=", ".join(self.teams)
        )
