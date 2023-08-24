from src.lineup.position import Position
from src.player.position_group_player import PositionGroupPlayer


class PositionGroup:
    players: list[PositionGroupPlayer]
    position: Position

    def __init__(self, pos_dict: dict) -> None:
        self.position = Position(pos_dict["position"])
        self.players = [PositionGroupPlayer(player) for player in pos_dict["players"]]
