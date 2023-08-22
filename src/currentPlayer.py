from src.teamOrMultiple import TeamOrMultiple
class CurrentPlayer:
    name: str = ""
    teams: list[TeamOrMultiple] = []
    position: str = ""

    def __init__(self, dict: dict, position: str) -> None:
        self.name = dict["name"]
        self.teams = map(TeamOrMultiple.fromString, dict["teams"])
        self.position = position

    def __str__(self) -> str:
        txt = "Player: {name}\n  Position: {position}\n  Teams: {teams}"
        return txt.format(
            name=self.name, position=self.position, teams=", ".join(self.teams)
        )

