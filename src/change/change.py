from src.change.change_type import ChangeType
from src.player.current_player import CurrentPlayer
from src.lineup.lineup import Lineup


class Change:
    change_type: ChangeType
    additions: list[CurrentPlayer]
    removals: list[str]

    def __init__(
        self,
        change_type: ChangeType,
        additions: list[CurrentPlayer] = None,
        removals: list[str] = None,
    ) -> None:
        self.change_type = ChangeType(change_type)
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
            change_type=ChangeType(change_dict.get("type")),
            additions=[
                CurrentPlayer.from_dict(addition)
                for addition in change_dict.get("additions", [])
            ],
            removals=change_dict.get("removals", []),
        )

    def apply(self, current_lineup: Lineup) -> Lineup:
        print(self.change_type)
        match self.change_type:
            case ChangeType.NOCHANGE:
                print(f"Not changing shit\n{current_lineup.pretty_print()}")
                return current_lineup
            case ChangeType.REMOVALS:
                return Lineup(
                    current_player
                    for current_player in current_lineup.players
                    if current_player.name not in self.removals
                )
            case ChangeType.ADDITIONS:
                current_lineup_players = list(current_lineup.players)
                player_positions = [player.position for player in current_lineup_players]
                for current_player in self.additions:
                    position_index = player_positions.index(current_player.position)
                    current_lineup_players.insert(position_index, current_player)
                return Lineup(current_lineup_players)
            case ChangeType.REPLACEMENT:
                removed = Change(ChangeType.REMOVALS, removals=self.removals).apply(
                    current_lineup
                )
                return Change(ChangeType.ADDITIONS, additions=self.additions).apply(
                    removed
                )

    def pretty_print(self) -> str:
        str_removals = ", ".join(self.removals)
        str_additions = ", ".join(player.name for player in self.additions)
        match self.change_type:
            case ChangeType.NOCHANGE:
                return "No change"
            case ChangeType.REMOVALS:
                return f"Removing {str_removals}"
            case ChangeType.ADDITIONS:
                return f"Adding {str_additions}"
            case ChangeType.REPLACEMENT:
                return f"Replacing {str_removals} with {str_additions}"
