from src.player.current_player_single_team import CurrentPlayerSingleTeam
from src.player.current_player import CurrentPlayer
from io import TextIOWrapper
import csv
from itertools import product


class PossibleLineup:
    THRESHOLD_FULL = 50
    THRESHOLD_HALF = 25

    data: list[CurrentPlayerSingleTeam] = []

    def __init__(self, data: list[CurrentPlayerSingleTeam]) -> None:
        self.data = data

    def __str__(self) -> str:
        return "\n".join([str(player) for player in self.data])

    def allValues(self) -> dict[str, int]:
        val: dict = {}
        for player in self.data:
            for tom in player.team:
                val[tom.name] = val.get(tom.name, 0) + int(tom.number)
        l = [(k, v) for k, v in val.items()]
        l.sort(reverse=True, key=lambda x: x[1])
        return dict(l)

    def value(self) -> tuple[str, int]:
        val = self.allValues()
        maxKey = max(val, key=val.get)
        maxValue = max(val.values())
        return (maxKey, maxValue)

    @staticmethod
    def compare(l1: "__class__", l2: "__class__") -> int:
        l1Val = l1.value()
        l2Val = l2.value()
        if l1Val[1] != l2Val[1]:
            return l1Val[1] - l2Val[1]
        else:
            return len(l2.allValues()) - len(l1.allValues())

    @staticmethod
    def fromRegularLineup(lineup: list[CurrentPlayer]) -> list["__class__"]:
        allPlayers = [
            CurrentPlayerSingleTeam.fromCurrentPlayer(player) for player in lineup
        ]

        return [PossibleLineup(potential) for potential in product(*allPlayers)]

    def writeToCsv(self, outfile: TextIOWrapper) -> None:
        writer = csv.writer(outfile, delimiter=",")
        writer.writerow(["Name", "Position", "Team"])
        for player in self.data:
            writer.writerow(
                [
                    player.name,
                    player.position.value,
                    "{team}".format(
                        team=" | ".join([str(team) for team in player.team])
                    ),
                ]
            )
        writer.writerow([None, None, None])
        bestValues = self.allValues()
        for key in bestValues.keys():
            writer.writerow([key.value, bestValues[key], None])
