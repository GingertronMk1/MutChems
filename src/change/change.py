from src.change.change_type import ChangeType
from src.player.current_player import CurrentPlayer


class Change:
    change_type: ChangeType
    additions: list[CurrentPlayer]
    removals: list[str]

    def __init__(
        self,
        change_type: str,
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
            change_type=change_dict["type"],
            additions=change_dict.get("additions", []),
            removals=change_dict.get("removals", []),
        )

    def apply(self, current_lineup: list[CurrentPlayer]) -> CurrentPlayer:
        match self.change_type:
            case ChangeType.REMOVALS:
                return [
                    current_player
                    for current_player in current_lineup
                    if current_player.name not in self.removals
                ]
            case ChangeType.ADDITIONS:
                player_positions = [player.position for player in current_lineup]
                for player in self.additions:
                    current_player = CurrentPlayer.from_dict(player)
                    position_index = player_positions.index(current_player.position)
                    current_lineup.insert(position_index, current_player)
                return current_lineup
            case ChangeType.REPLACEMENT:
                removed = Change(ChangeType.REMOVALS, removals=self.removals).apply(
                    current_lineup
                )
                return Change(ChangeType.ADDITIONS, additions=self.additions).apply(
                    removed
                )
