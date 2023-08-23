from src.position import Position
from src.player.position_group_player import PositionGroupPlayer


class PositionGroup:
    players: list[PositionGroupPlayer]
    position: Position

    def __init__(self, dict: dict) -> None:
        self.position = Position(dict["position"])
        self.players = [PositionGroupPlayer(player) for player in dict["players"]]
