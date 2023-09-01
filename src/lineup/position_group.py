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
    def from_dict(initial_dict: dict) -> "PositionGroup":
        """Create from dict"""
        position: str | None = initial_dict.get("position")
        if position is None:
            raise ValueError("No position found in dict")
        return PositionGroup(
            position=Position.get_from_abbreviation(position),
            players=[
                PositionGroupPlayer.from_dict(player)
                for player in initial_dict.get("players", [])
            ],
        )

    def add_player(self, player: PositionGroupPlayer) -> "PositionGroup":
        """Add a player to the top of the group"""
        self.players.insert(0, player)
        return self
