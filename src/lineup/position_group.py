"""A group of players joined by position"""
from dataclasses import dataclass
from src.lineup.position import Position
from src.player.position_group_player import PositionGroupPlayer


@dataclass
class PositionGroup:
    """A group of players joined by position"""

    position: Position
    players: list[PositionGroupPlayer]

    @staticmethod
    def from_dict(initial_dict: dict) -> "__class__":
        """Create from dict"""
        return PositionGroup(
            position=Position(initial_dict.get("position")),
            players=[
                PositionGroupPlayer.from_dict(player)
                for player in initial_dict.get("players", [])
            ],
        )

    def add_player(self, player: PositionGroupPlayer) -> "__class__":
        """Add a player to the top of the group"""
        self.players.insert(0, player)
