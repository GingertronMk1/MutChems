"""The value of a Variation"""
from copy import deepcopy
from dataclasses import dataclass
from functools import reduce
from statistics import mean
from src.variation.variation import Variation
from src.team.team import Team
from src.lineup.lineup import Lineup
from src.player.lineup_player import LineupPlayer


@dataclass
class ValueHelper:
    """A helper class to accumulate a reducing list"""

    variation: Variation
    number_of_variations: int
    iteration: int = 0
    current_percent: int = 0

    @staticmethod
    def from_list(variations: list[Variation]) -> "ValueHelper":
        """Create from a list of Variations"""
        return ValueHelper(variations[0], len(variations))


class Value:
    """The value of a Variation"""

    tiers: list[tuple]

    def __init__(self, variation: Variation) -> None:
        all_teams = [t for player in variation.players for t in player.team.expand()]
        individuals = set(all_teams)
        compressed_toms = [
            (t, len([x for x in all_teams if x == t])) for t in individuals
        ]
        compressed_toms.sort(key=lambda x: x[1], reverse=True)
        self.numbers = compressed_toms

    def get_tiers(self) -> list[tuple[Team, int]]:
        """Get the tiers of the teams in the Variation"""
        return [(t, l // 5) for (t, l) in self.numbers]

    def get_max_tier(self) -> int:
        """What is the highest team value"""
        return max(t[1] for t in self.get_tiers())

    def get_number_at_max_tier(self) -> int:
        """How many are at that tier"""
        max_tier = self.get_max_tier()
        return len([ts for ts in self.get_tiers() if ts[1] == max_tier])

    def get_mean(self) -> float:
        """Get the mean of squared values"""
        return mean(t[1] ** 2 for t in self.numbers)

    @staticmethod
    def compare_variations(variation_1: Variation, variation_2: Variation) -> float:
        """Compare variations by value"""
        variation_1_value = Value(variation_1)
        variation_2_value = Value(variation_2)

        tier_diff = variation_1_value.get_max_tier() - variation_2_value.get_max_tier()

        if tier_diff != 0:
            return tier_diff

        number_at_tier_diff = (
            variation_1_value.get_number_at_max_tier()
            - variation_2_value.get_number_at_max_tier()
        )

        if number_at_tier_diff != 0:
            return number_at_tier_diff

        mean_diff = variation_1_value.get_mean() - variation_2_value.get_mean()
        return mean_diff

    @staticmethod
    def __reduce_helper(
        accumulator: ValueHelper, variation_2: Variation
    ) -> ValueHelper:
        """Used in the reducing of Variations to a single best one"""
        iteration = accumulator.iteration + 1
        current_percent = accumulator.current_percent
        variation_1 = accumulator.variation
        number_of_variations = accumulator.number_of_variations
        ret_variation = variation_1
        if Value.compare_variations(variation_1, variation_2) < 0:
            ret_variation = variation_2
        new_percent = (100 * iteration) // number_of_variations
        if new_percent > current_percent:
            current_percent = new_percent
            print(f"\t{new_percent}% done ({iteration:,} / {number_of_variations:,})")
        accumulator.variation = ret_variation
        accumulator.iteration = iteration
        accumulator.current_percent = current_percent
        return accumulator

    @staticmethod
    def find_best_possible_variation(variations: list[Variation]) -> Variation:
        """Finding the best of a list of Variations"""
        reduced = reduce(
            Value.__reduce_helper, variations, ValueHelper.from_list(variations)
        )
        best_variation = reduced.variation
        return best_variation

    @staticmethod
    def get_best_lineup_variation(
        original_lineup: Lineup, iteration: int = 0
    ) -> Variation:
        """Get best variation from a given lineup"""
        original_lineup_players = deepcopy(original_lineup.players)
        if Team.NO_TEAM in original_lineup.all_teams_list():
            raise ValueError("NoTeam players have made it into the start")
        filtered_lineup: Lineup = original_lineup.filter_to_n_options()
        print(f"Round {iteration}: {filtered_lineup.num_options()} options")
        best_possible = Value.find_best_possible_variation(
            filtered_lineup.to_variations()
        )
        if best_possible.contains_no_team_players():
            for index, player in enumerate(best_possible.players):
                player_teams = player.expand_teams()
                if Team.NO_TEAM not in player_teams:
                    new_player = LineupPlayer(
                        player.name,
                        player.position,
                        [player.team],
                    )
                    original_lineup_players[index] = new_player
            new_lineup = Lineup(original_lineup_players)
            return Value.get_best_lineup_variation(new_lineup, iteration + 1)
        return best_possible
