import json
import itertools
import functools
import math
from enum import Enum
import csv

class Team(Enum):
  BEARS = 'Bears'
  BENGALS = 'Bengals'
  BILLS = 'Bills'
  BRONCOS = 'Broncos'
  BROWNS = 'Browns'
  BUCCANEERS = 'Buccaneers'
  CARDINALS = 'Cardinals'
  CHARGERS = 'Chargers'
  CHIEFS = 'Chiefs'
  COLTS = 'Colts'
  COMMANDERS = 'Commanders'
  COWBOYS = 'Cowboys'
  DOLPHINS = 'Dolphins'
  EAGLES = 'Eagles'
  FALCONS = 'Falcons'
  GIANTS = 'Giants'
  JAGUARS = 'Jaguars'
  JETS = 'Jets'
  LIONS = 'Lions'
  NINERS = '49ers'
  PACKERS = 'Packers'
  PANTHERS = 'Panthers'
  PATRIOTS = 'Patriots'
  RAIDERS = 'Raiders'
  RAMS = 'Rams'
  RAVENS = 'Ravens'
  SAINTS = 'Saints'
  SEAHAWKS = 'Seahawks'
  STEELERS = 'Steelers'
  TEXANS = 'Texans'
  TITANS = 'Titans'
  VIKINGS = 'Vikings'


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

class TeamOrMultiple:
    name: Team
    number: int = 1
    def __init__(self, name: Team, number: int = 1) -> None:
        self.name = name
        self.number = number

    @staticmethod
    def fromString(string: str) -> list['__class__']:
      if '|' in string:
          return [tom for row in (TeamOrMultiple.fromString, string.split("|")) for tom in row]
      if '.' in string:
          tokens = string.split(".")
          return [TeamOrMultiple(Team(tokens[0]), tokens[1])]
      return [TeamOrMultiple(Team(string))]

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
            for tom in TeamOrMultiple.fromString(player.team):
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

allPossibles = PossibleLineup.sort(allPossibles)

bestPossible = allPossibles[-1]

with open('output.csv', 'w') as csvfile:
    writer = csv.writer(csvfile, delimiter=",")
    writer.writerow(['Name', 'Position', 'Team'])
    [
        writer.writerow(
          [player.name, player.position, player.team]
        ) for player in bestPossible.data
    ]
