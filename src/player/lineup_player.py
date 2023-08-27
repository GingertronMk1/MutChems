"""A base player object"""
from dataclasses import dataclass
from src.team.team_or_multiple import TeamOrMultiple
from src.lineup.position import Position
from src.player.position_group_player import PositionGroupPlayer
from src.lineup.position_group import PositionGroup
from src.team.team import Team
from src.team.team_and_number import TeamAndNumber


@dataclass
class LineupPlayer:
    """A base player object"""

    name: str = ""
    teams: list[TeamOrMultiple] = []
    position: Position

    @staticmethod
    def from_dict(initial_dict: dict) -> "__class__":
        """Creating from a dict"""
        return LineupPlayer(
            name=initial_dict["name"],
            teams=TeamOrMultiple.from_strings(initial_dict["teams"]),
            position=Position(initial_dict["position"]),
        )

    @staticmethod
    def from_position_group_player(
        pgp: PositionGroupPlayer, pos: Position
    ) -> "__class__":
        """Creating from a position group player"""
        return LineupPlayer(pgp.name, pgp.teams, pos)

    @staticmethod
    def from_position_group(pos_group: PositionGroup) -> list["__class__"]:
        """Creating from a position group"""
        return [
            LineupPlayer.from_position_group_player(player, pos_group.position)
            for player in pos_group.players
        ]

    @staticmethod
    def from_position_group_dict(pos_group_dict: dict) -> list["__class__"]:
        """From a position group dict - basically skipping the position group part"""
        return LineupPlayer.from_position_group(PositionGroup.from_dict(pos_group_dict))

    def __str__(self) -> str:
        """Nicely print"""
        return (
            f"Player: {self.name} | "
            f"  Position: {self.position} | "
            f"  Teams: {', '.join(str(t_o_m) for t_o_m in self.teams)}"
        )

    def filter_out_teams(self, teams: list[Team]) -> "__class__":
        """Filtering out teams to cut down the number of possible variations"""
        if len(self.teams) < 2:
            return self
        self.teams = [t_o_m for t_o_m in self.teams if not t_o_m.contains_teams(teams)]
        if not self.teams:
            t_a_n = TeamAndNumber(Team.NO_TEAM, 0)
            self.teams = [TeamOrMultiple(children=[t_a_n])]
        return self
