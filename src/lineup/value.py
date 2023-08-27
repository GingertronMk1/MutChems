"""The value of a Variation"""
from dataclasses import dataclass
from functools import reduce
from src.lineup.variation import Variation


@dataclass
class ValueHelper:
    """A helper class to accumulate a reducing list"""

    variation: Variation
    number_of_variations: int
    iteration: int = 0
    current_percent: int = 0

    @staticmethod
    def from_list(variations: list[Variation]) -> "__class__":
        """Create from a list of Variations"""
        return ValueHelper(variations[0], len(variations))


class Value:
    """The value of a Variation"""

    tiers: list[tuple]

    def __init__(self, variation: Variation) -> None:
        all_teams = [t for player in variation.data for t in player.team.expand()]
        individuals = set(all_teams)
        compressed_toms = [
            (t, len([x for x in all_teams if x == t])) for t in individuals
        ]
        compressed_toms.sort(key=lambda x: x[1], reverse=True)
        self.tiers = [(t, l // 5) for (t, l) in compressed_toms]

    def get_max_tier(self) -> int:
        """What is the highest team value"""
        return max(t[1] for t in self.tiers)

    def get_number_at_max_tier(self) -> int:
        """How many are at that tier"""
        max_tier = self.get_max_tier()
        return len([ts for ts in self.tiers if ts[1] == max_tier])

    @staticmethod
    def compare_variations(variation_1: Variation, variation_2: Variation) -> int:
        """Compare variations by value"""
        variation_1_value = Value(variation_1)
        variation_2_value = Value(variation_2)

        tier_diff = variation_1_value.get_max_tier() - variation_2_value.get_max_tier()

        number_at_tier_diff = (
            variation_1_value.get_number_at_max_tier()
            - variation_2_value.get_number_at_max_tier()
        )
        if tier_diff != 0:
            return tier_diff
        return number_at_tier_diff

    @staticmethod
    def __reduce_helper(
        accumulator: ValueHelper, variation_2: Variation
    ) -> ValueHelper:
        """Used in the reducing of Variations to a single best one"""
        iteration = accumulator.iteration
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
        return reduced.variation
