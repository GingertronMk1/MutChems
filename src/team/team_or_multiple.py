"""A team or multiple teams for chemistries"""
from src.team.team import Team
from src.team.team_and_number import TeamAndNumber
from itertools import takewhile


class TeamOrMultiple:
    """A team or multiple teams for chemistries"""

    children: list[TeamAndNumber] = []

    def __init__(self, children: list[TeamAndNumber] = []) -> None:
        self.children = children

    @staticmethod
    def from_string(string: str) -> "__class__":
        return TeamOrMultiple(children=TeamAndNumber.from_string(string))

    @staticmethod
    def from_strings(strings: list[str]) -> list["__class__"]:
        return_val = []
        for string in strings:
            t_o_m = TeamOrMultiple.from_string(string)
            all_teams = [t_a_n.team for t_a_n in t_o_m.children]
            if Team.ALL32 in all_teams:
              all_32_index = all_teams.index(Team.ALL32)
              all_team_tan = t_o_m.children[all_32_index]
              other_teams = [t_a_n for t_a_n in t_o_m.children if t_a_n.team != Team.ALL32]
              for team in Team.all_teams():
                  return_val.append(TeamOrMultiple([TeamAndNumber(team, all_team_tan.number)] + other_teams))
            else:
              return_val.append(t_o_m)
        return return_val

    def expand(self) -> list[Team]:
        # print([type(t) for t in self.children])
        return [t for child in self.children for t in child.expand()]

    def __str__(self) -> str:
      return ' | '.join(str(t_a_n) for t_a_n in self.children)