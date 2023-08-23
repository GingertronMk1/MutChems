from src.team_or_multiple import TeamOrMultiple
from src.position import Position
from src.team import Team
from src.player.position_group_player import PositionGroupPlayer
from src.lineup.position_group import PositionGroup


class CurrentPlayer:
    name: str = ""
    teams: list[TeamOrMultiple] = []
    position: Position

    def __init__(
        self, name: str, teams: list[TeamOrMultiple], position: Position
    ) -> None:
        self.name = name
        self.teams = teams
        self.position = position

    @staticmethod
    def fromDict(dict: dict) -> "__class__":
        return CurrentPlayer(
            name=dict["name"],
            teams=[TeamOrMultiple.fromString(s) for s in dict["teams"]],
            position=Position(dict["position"]),
        )

    @staticmethod
    def fromPositionGroupPlayer(pgp: PositionGroupPlayer, pos: Position) -> "__class__":
        return CurrentPlayer(pgp.name, pgp.teams, pos)

    @staticmethod
    def fromPositionGroup(posGroup: PositionGroup) -> list["__class__"]:
        return [
            CurrentPlayer.fromPositionGroupPlayer(player, posGroup.position)
            for player in posGroup.players
        ]

    def __str__(self) -> str:
        txt = "Player: {name}\n  Position: {position}\n  Teams: {teams}"
        return txt.format(
            name=self.name,
            position=self.position,
            teams=", ".join([str(team) for teams in self.teams for team in teams]),
        )
