import json
import itertools
import functools
import math


class CurrentPlayer:
    name: str = ""
    teams: list[str] = []
    position: str = ""

    def __init__(self, dict, position) -> None:
        self.name = dict["name"]
        self.teams = dict["teams"]
        self.position = position

    def __str__(self) -> str:
        txt = "Player: {name}\n  Position: {position}\n  Teams: {teams}"
        return txt.format(
            name=self.name, position=self.position, teams=", ".join(self.teams)
        )


class CurrentPlayerSingleTeam:
    name: str = ""
    team: str = ""
    position: str = ""

    def __init__(self, name: str, team: str, position: str) -> None:
        self.name = name
        self.team = team
        self.position = position

    @staticmethod
    def fromCurrentPlayer(cp) -> list["__class__"]:
        return [
            CurrentPlayerSingleTeam(cp.name, team, cp.position) for team in cp.teams
        ]

    def __repr__(self) -> str:
        return "{name} | {team} | {position}".format(
            name=self.name, team=self.team, position=self.position
        )


class PossibleLineup:
    data: list[CurrentPlayerSingleTeam] = []

    def __init__(self, data: list[CurrentPlayerSingleTeam]) -> None:
        self.data = data

    def __str__(self) -> str:
        return "\n".join(map(str, self.data))

    def allValues(self) -> dict:
        val: dict = {}
        for player in self.data:
            tokens = player.team.split(".")
            currentTeamVal = val.get(tokens[0], 0)
            if len(tokens) == 2:
                val[tokens[0]] = currentTeamVal + int(tokens[1])
            else:
                val[tokens[0]] = currentTeamVal + 1
        return val


    def value(self) -> tuple[str, int]:
        val = self.allValues()
        return (max(val), max(val.values()))
    
    @staticmethod
    def compare(l1: '__class__', l2: '__class__') -> int:
        l1Val = l1.value()
        l2Val = l2.value()
        if l1Val[1] != l2Val[1]:
          return math.copysign(l1Val[1], l2Val[1])
        else:
          return math.copysign(len(l1.allValues()), len(l2.allValues()))

    @staticmethod
    def sort(ls: list['__class__']) -> list['__class__']:
        return sorted(ls, key=functools.cmp_to_key(PossibleLineup.compare))


with open("./data/team.json", "r") as f:
    data = json.load(f)

allPlayers = []

for position in data["current"]:
    for player in position["players"]:
        allPlayers.append(CurrentPlayer(player, position["position"]))

allPotentials = []

for player in allPlayers:
    allPotentials.append(CurrentPlayerSingleTeam.fromCurrentPlayer(player))

allPotentials = itertools.product(*allPotentials)

allPossibles = []
for potential in allPotentials:
    allPossibles.append(PossibleLineup(list(potential)))

PossibleLineup.sort(allPossibles)

for possible in allPossibles:
    print("{value}".format(value=possible.value()))
