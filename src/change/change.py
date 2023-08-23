from src.change.change_type import ChangeType
from src.player.current_player import CurrentPlayer


class Change:
    type: ChangeType
    additions: list[CurrentPlayer]
    removals: list[str]

    def __init__(
        self, type: str, additions: list[CurrentPlayer] = [], removals: list[str] = []
    ) -> None:
        self.type = ChangeType(type)
        self.additions = additions
        self.removals = removals

    @staticmethod
    def fromDict(dict: dict) -> "__class__":
        return Change(
            type=dict["type"],
            additions=dict.get("additions", []),
            removals=dict.get("removals", []),
        )

    def apply(self, currentLineup: list[CurrentPlayer]) -> CurrentPlayer:
        match self.type:
            case ChangeType.REMOVALS:
                return [
                    currentPlayer
                    for currentPlayer in currentLineup
                    if currentPlayer.name not in self.removals
                ]
            case ChangeType.ADDITIONS:
                playerPositions = [player.position for player in currentLineup]
                for player in self.additions:
                    currentPlayer = CurrentPlayer.fromDict(player)
                    positionIndex = playerPositions.index(currentPlayer.position)
                    currentLineup.insert(positionIndex, currentPlayer)
                return currentLineup
            case ChangeType.REPLACEMENT:
                removed = Change(ChangeType.REMOVALS, removals=self.removals).apply(
                    currentLineup
                )
                return Change(ChangeType.ADDITIONS, additions=self.additions).apply(
                    removed
                )
