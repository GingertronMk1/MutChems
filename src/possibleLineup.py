from src.currentPlayerSingleTeam import CurrentPlayerSingleTeam
import functools
import math

class PossibleLineup:
    data: list[CurrentPlayerSingleTeam] = []

    def __init__(self, data: list[CurrentPlayerSingleTeam]) -> None:
        self.data = data

    def __str__(self) -> str:
        return "\n".join(map(str, self.data))

    def allValues(self) -> dict:
        val: dict = {}
        for player in self.data:
            for tom in player.team:
              val[tom.name] = val.get(tom.name, 0) + int(tom.number)
        return val

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
            return math.copysign(len(l1.allValues()), len(l2.allValues()))

    @staticmethod
    def sort(ls: list["__class__"]) -> list["__class__"]:
        return sorted(ls, key=functools.cmp_to_key(PossibleLineup.compare))


