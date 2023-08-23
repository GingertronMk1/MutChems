from src.teamOrMultiple import TeamOrMultiple
from src.position import Position
from src.team import Team
from src.player.positionGroupPlayer import PositionGroupPlayer
from src.lineup.positionGroup import PositionGroup


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
            name=self.name, position=self.position, teams=", ".join(self.teams)
        )
