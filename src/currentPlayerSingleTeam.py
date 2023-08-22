from src.teamOrMultiple import TeamOrMultiple
from src.currentPlayer import CurrentPlayer

class CurrentPlayerSingleTeam:
    name: str = ""
    team: TeamOrMultiple
    position: str = ""

    def __init__(self, name: str, team: TeamOrMultiple, position: str) -> None:
        self.name = name
        self.team = team
        self.position = position

    @staticmethod
    def fromCurrentPlayer(cp: CurrentPlayer) -> list["__class__"]:
        return [
            CurrentPlayerSingleTeam(cp.name, team, cp.position) for team in cp.teams
        ]

    def __repr__(self) -> str:
        return "{name} | {team} | {position}".format(
            name=self.name, team=self.team, position=self.position
        )

