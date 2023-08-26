from src.change.change_type import ChangeType
from src.player.current_player import CurrentPlayer
from src.lineup.lineup import Lineup


class Change:
    additions: list[CurrentPlayer]
    removals: list[str]

    def __init__(
        self,
        additions: list[CurrentPlayer] = None,
        removals: list[str] = None,
    ) -> None:
        if additions is None:
            self.additions = []
        else:
            self.additions = additions
        if removals is None:
            self.removals = []
        else:
            self.removals = removals

    @staticmethod
    def from_dict(change_dict: dict) -> "__class__":
        return Change(
            additions=[
                CurrentPlayer.from_dict(addition)
                for addition in change_dict.get("additions", [])
            ],
            removals=change_dict.get("removals", []),
        )

    def apply(self, current_lineup: Lineup) -> Lineup:

        removed_players = [
            current_player
            for current_player in current_lineup.players
            if current_player.name not in self.removals
        ]
        player_positions = [player.position for player in removed_players]
        for current_player in self.additions:
            position_index = player_positions.index(current_player.position)
            removed_players.insert(position_index, current_player)
        return Lineup(removed_players)

    def pretty_print(self) -> str:
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
        len_additions = len(self.additions)
        len_removals = len(self.removals)
        if len_additions and len_removals:
            return ChangeType.REPLACEMENT
        elif len_additions:
            return ChangeType.ADDITIONS
        elif len_removals:
            return ChangeType.REMOVALS
        return ChangeType.NOCHANGE

    @staticmethod
    def no_change_change() -> '__class__':
        return Change(ChangeType.NOCHANGE)
