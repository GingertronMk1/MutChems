from src.position import Position
from src.player.positionGroupPlayer import PositionGroupPlayer


class PositionGroup:
    players: list[PositionGroupPlayer]
    position: Position

    def __init__(self, dict: dict) -> None:
        self.position = Position(dict["position"])
        self.players = map(PositionGroupPlayer, dict["players"])
