import json
import itertools

class CurrentPlayer:
  name: str = ""
  teams: list[str] = []
  position: str = ""

  def __init__(self, dict, position) -> None:
      self.name = dict["name"]
      self.teams = dict["teams"]
      self.position = position
  
  def __str__(self) -> str:
    txt = 'Player: {name}\n  Position: {position}\n  Teams: {teams}'
    return txt.format(name=self.name, position=self.position, teams=', '.join(self.teams))


class CurrentPlayerSingleTeam:
  name: str = ""
  team: str = ""
  position: str = ""

  def __init__(self, name: str, team: str, position: str) -> None:
    self.name = name
    self.team = team
    self.position = position

  @staticmethod
  def fromCurrentPlayer(cp) -> list['__class__']:
    return [CurrentPlayerSingleTeam(cp.name, team, cp.position) for team in cp.teams]

  def __repr__(self) -> str:
    return "{name} | {team} | {position}".format(name=self.name, team=self.team, position=self.position)

class PossibleLineup:
  data: list[CurrentPlayerSingleTeam] = []

  def __init__(self, data: list[CurrentPlayerSingleTeam]) -> None:
    self.data = data

  def __str__(self) -> str:
    return "\n".join(map(str, self.data))

  def value(self) -> tuple[str, int]:
    val: dict = {}
    for player in self.data:
      tokens = player.team.split(".")
      currentTeamVal = val.get(tokens[0], 0)
      if len(tokens) == 2:
        val[tokens[0]] = currentTeamVal + int(tokens[1])
      else:
        val[tokens[0]] = currentTeamVal + 1
    return (max(val), max(val.values()))



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

allPossibles.sort(key = lambda x: x.value()[1])

for possible in allPossibles:
  print("{possible}\n{value}\n\n".format(possible = possible, value = possible.value()))