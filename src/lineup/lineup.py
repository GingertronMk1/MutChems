from src.player.current_player import CurrentPlayer
from src.lineup.variation import Variation
from src.player.current_player_single_team import CurrentPlayerSingleTeam
from itertools import product
from functools import reduce
from src.team.team import Team


class Lineup:
    players: list[CurrentPlayer]

    def __init__(self, players: list[CurrentPlayer]) -> None:
        self.players = players

    def to_variations(self) -> list[Variation]:
        all_players = [
            CurrentPlayerSingleTeam.from_current_player(player)
            for player in self.players
        ]

        return [Variation(potential) for potential in product(*all_players)]

    def filter_out_teams(self, teams: list[Team]) -> "__class__":
        self.players = [player.filter_out_teams(teams) for player in self.players]
        return self

    def pretty_print(self) -> str:
        return "\n".join(str(player) for player in self.players)

    def all_teams(self) -> dict:
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

    def all_teams_below_threshold(self, threshold: int = 5) -> list[Team]:
        return_val = []
        for team, number in self.all_teams().items():
            if number < threshold:
                return_val.append(team)
        return return_val

    def num_options(self) -> int:
        return reduce(
            lambda x, y: x * y, [len(player.teams) for player in self.players], 1
        )

    def filter_to_n_options(self, n=1_000_000) -> "__class__":
        threshold = 0
        while self.num_options() > n:
            self = self.filter_out_teams(self.all_teams_below_threshold(threshold))
            threshold += 1
        print(threshold)
        return self
