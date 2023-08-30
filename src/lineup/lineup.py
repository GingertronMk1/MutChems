"""A list of players"""
from dataclasses import dataclass
from itertools import product
from functools import reduce
from src.player.lineup_player import LineupPlayer
from src.variation.variation import Variation
from src.player.variation_player import VariationPlayer
from src.team.team import Team
from src.player.position_group_player import PositionGroupPlayer
from src.lineup.position_group import PositionGroup
from src.lineup.position import Position


@dataclass
class Lineup:
    """A list of players"""

    players: list[LineupPlayer]

    def to_variations(self) -> list[Variation]:
        """Convert to all possible variations"""
        all_players = [
            VariationPlayer.from_lineup_player(player) for player in self.players
        ]

        return [Variation(potential) for potential in product(*all_players)]

    def filter_out_teams(self, teams: list[Team]) -> "__class__":
        """Filter out a given list of Teams"""
        self.players = [player.filter_out_teams(teams) for player in self.players]
        return self

    def pretty_print(self) -> str:
        """Print nicely"""
        return "\n".join(str(player) for player in self.players)

    def all_teams(self) -> dict:
        """Get all teams in the lineup"""
        all_player_teams = {}
        for player in self.players:
            all_teams = [
                team
                for t_o_m in player.teams
                for t_a_n in t_o_m.children
                for team in t_a_n.expand()
            ]
            unique_teams = set()
            for team in all_teams:
                unique_teams.add(team)
            for team in unique_teams:
                num_of_team = all_player_teams.get(team, 0)
                all_player_teams[team] = num_of_team + 1
        return all_player_teams

    def all_teams_list(self) -> list[Team]:
        """Get all teams in a Lineup"""
        return [team for player in self.players for team in player.expand_teams()]

    def all_teams_below_threshold(self, threshold: int = 5) -> list[Team]:
        """Get all teams that have fewer than `threshold` instances"""
        return_val = []
        for team, number in self.all_teams().items():
            if number < threshold:
                return_val.append(team)
        return return_val

    def num_options(self) -> int:
        """How many possible Variations do we have?"""
        return reduce(
            lambda x, y: x * y, [len(player.teams) for player in self.players], 1
        )

    def filter_to_n_options(self, max_options=1_000_000) -> "__class__":
        """Filter the available teams until we are under a given possible number of Variations"""
        threshold = 0
        while self.num_options() > max_options:
            filtered = self.filter_out_teams(self.all_teams_below_threshold(threshold))
            self.players = filtered.players
            threshold += 1
        return self

    @staticmethod
    def from_position_groups(groups: list[PositionGroup]) -> "__class__":
        """Get from position groups"""
        players = list(
            player
            for posGroup in groups
            for player in LineupPlayer.from_position_group(posGroup)
        )
        except_values = []
        for position in list(Position):
            if position not in [player.position for player in players]:
                except_values.append(position)
        if len(except_values) > 0:
            raise ValueError(
                f"Missing {','.join(position.value for position in except_values)}"
            )
        return Lineup(players)

    def to_position_groups(self) -> list[PositionGroup]:
        """Convert back to position groups"""
        ret_values = []
        for position in list(Position):
            ret_values.append(
                PositionGroup(
                    position,
                    [
                        PositionGroupPlayer(player.name, player.teams)
                        for player in self.players
                        if player.position == position
                    ],
                )
            )
        return ret_values
