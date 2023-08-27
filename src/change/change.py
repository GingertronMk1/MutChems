"""A change to a Lineup"""
from dataclasses import dataclass
from enum import Enum
from src.player.lineup_player import LineupPlayer
from src.lineup.lineup import Lineup


class ChangeType(Enum):
    """The type of a change"""

    REMOVALS = "removals"
    ADDITIONS = "additions"
    REPLACEMENT = "replacement"
    NOCHANGE = "no_change"


@dataclass
class Change:
    """A change to a Lineup"""

    additions: list[LineupPlayer] = []
    removals: list[str] = []

    @staticmethod
    def from_dict(change_dict: dict) -> "__class__":
        """Generate a Change from a dict"""
        return Change(
            additions=[
                LineupPlayer.from_dict(addition)
                for addition in change_dict.get("additions", [])
            ],
            removals=change_dict.get("removals", []),
        )

    def apply(self, current_lineup: Lineup) -> Lineup:
        """Apply a change to a lineup"""
        removed_players = [
            lineup_player
            for lineup_player in current_lineup.players
            if lineup_player.name not in self.removals
        ]
        player_positions = [player.position for player in removed_players]
        for lineup_player in self.additions:
            position_index = player_positions.index(lineup_player.position)
            removed_players.insert(position_index, lineup_player)
        return Lineup(removed_players)

    def pretty_print(self) -> str:
        """Nicely print a change"""
        str_removals = ", ".join(self.removals)
        str_additions = ", ".join(player.name for player in self.additions)
        match self.get_type():
            case ChangeType.NOCHANGE:
                return "No change"
            case ChangeType.REMOVALS:
                return f"Removing {str_removals}"
            case ChangeType.ADDITIONS:
                return f"Adding {str_additions}"
            case ChangeType.REPLACEMENT:
                return f"Replacing {str_removals} with {str_additions}"

    def get_type(self) -> ChangeType:
        """Dynamically get the type of a change"""
        len_additions = len(self.additions)
        len_removals = len(self.removals)
        if len_additions and len_removals:
            return ChangeType.REPLACEMENT
        if len_additions:
            return ChangeType.ADDITIONS
        if len_removals:
            return ChangeType.REMOVALS
        return ChangeType.NOCHANGE
