from src.lineup.position import Position
from src.player.position_group_player import PositionGroupPlayer

class PositionGroup:
    players: list[PositionGroupPlayer]
    position: Position

    def __init__(
        self, position: Position, players: list[PositionGroupPlayer] = None
    ) -> None:
        self.position = position
        self.players = players

    @staticmethod
    def from_dict(initial_dict: dict) -> "__class__":
        return PositionGroup(
            position=Position(initial_dict.get("position")),
            players=[
                PositionGroupPlayer.from_dict(player)
                for player in initial_dict.get("players", [])
            ],
        )
