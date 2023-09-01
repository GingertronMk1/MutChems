"""A change to a Lineup"""
from dataclasses import dataclass, field
from enum import Enum
from src.player.lineup_player import LineupPlayer
from src.lineup.lineup import Lineup
from src.lineup.position import Position
from src.player.position_group_player import PositionGroupPlayer


class ChangeType(Enum):
    """The type of a change"""

    REMOVALS = "removals"
    ADDITIONS = "additions"
    REPLACEMENT = "replacement"
    NOCHANGE = "no_change"


@dataclass
class Change:
    """A change to a Lineup"""

    additions: list[LineupPlayer] = field(default_factory=list)
    removals: list[str] = field(default_factory=list)

    @staticmethod
    def from_dict(change_dict: dict) -> "Change":
        """Generate a Change from a dict"""
        return Change(
            additions=[
                LineupPlayer.from_dict(addition)
                for addition in change_dict.get("additions", [])
            ],
            removals=change_dict.get("removals", []),
        )

    def apply(self, current_lineup: Lineup) -> Lineup:
        """
        Apply a change to a lineup, first by removing the removals
        then by adding the additions
        """

        for player in self.removals:
            if player not in [player.name for player in current_lineup.players]:
                raise ValueError(f"{player} not in lineup, so cannot be removed")
        removed_players = [
            lineup_player
            for lineup_player in current_lineup.players
            if lineup_player.name not in self.removals
        ]
        new_position_groups = Lineup(removed_players).to_position_groups()
        for lineup_player in self.additions:
            position_index = list(Position).index(lineup_player.position)
            current_position_group = new_position_groups[position_index]
            current_position_group.add_player(
                PositionGroupPlayer(lineup_player.name, lineup_player.teams)
            )
            new_position_groups[position_index] = current_position_group
        return Lineup.from_position_groups(new_position_groups)

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
        return "Somehow you've got something entirely else"

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
