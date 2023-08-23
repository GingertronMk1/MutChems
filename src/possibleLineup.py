from src.player.currentPlayerSingleTeam import CurrentPlayerSingleTeam
import functools
import math


class PossibleLineup:
    data: list[CurrentPlayerSingleTeam] = []

    def __init__(self, data: list[CurrentPlayerSingleTeam]) -> None:
        self.data = data

    def __str__(self) -> str:
        return "\n".join(map(str, self.data))

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
            return math.copysign(l1Val[1], l2Val[1])
        else:
            return math.copysign(len(l2.allValues()), len(l1.allValues()))
