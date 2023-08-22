from src.team import Team

class TeamOrMultiple:
    name: Team
    number: int = 1
    def __init__(self, name: Team, number: int = 1) -> None:
        self.name = name
        self.number = number

    @staticmethod
    def fromString(string: str) -> list['__class__']:
      if '|' in string:
          return [tom for row in map(TeamOrMultiple.fromString, string.split("|")) for tom in row]
      if '.' in string:
          tokens = string.split(".")
          return [TeamOrMultiple(Team(tokens[0]), tokens[1])]
      return [TeamOrMultiple(Team(string))]

    def __str__(self) -> str:
        return "{team}.{number}".format(team=self.name, number=self.number)

