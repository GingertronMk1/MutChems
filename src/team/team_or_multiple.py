"""A team or multiple teams for chemistries"""
from src.team.team import Team
from src.team.team_and_number import TeamAndNumber
import itertools


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
            return_val.extend(t_o_m.expand_all_32())
        return return_val

    def expand_all_32(self) -> list["__class__"]:
        all_teams = [t_a_n.team for t_a_n in self.children]
        if Team.ALL32 in all_teams:
            all_32_index = all_teams.index(Team.ALL32)
            all_team_tan = self.children[all_32_index]
            other_teams = self.children
            del other_teams[all_32_index]
            return [
                t_o_m
                for team in Team.all_teams()
                for t_o_m in TeamOrMultiple(
                    [TeamAndNumber(team, all_team_tan.number)] + other_teams
                ).expand_all_32()
            ]
        else:
            return [self.normalise()]

    def normalise(self) -> "__class__":
        acc_dict = {}
        for team_and_number in self.children:
            team_total = acc_dict.get(team_and_number.team, 0)
            acc_dict[team_and_number.team] = team_total + team_and_number.number
        self.children = [TeamAndNumber(t, n) for t, n in acc_dict.items()]
        return self

    def expand(self) -> list[Team]:
        # print([type(t) for t in self.children])
        return [t for child in self.children for t in child.expand()]

    def __str__(self) -> str:
        return " | ".join(str(t_a_n) for t_a_n in self.children)

    def contains_team(self, team: Team) -> bool:
        return all(team == child.team for child in self.children)
