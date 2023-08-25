from src.lineup.possible_lineup import PossibleLineup


class Value:
    tiers: list[tuple]

    def __init__(self, possible_lineup: PossibleLineup) -> None:
        all_teams = [t for player in possible_lineup.data for t in player.team.expand()]
        individuals = set(all_teams)
        compressed_toms = [
            (t, len([x for x in all_teams if x == t])) for t in individuals
        ]
        compressed_toms.sort(key=lambda x: x[1], reverse=True)
        self.tiers = [(t, l // 5) for (t, l) in compressed_toms]

    def get_max_tier(self) -> int:
        return max([t[1] for t in self.tiers])

    def get_number_at_max_tier(self) -> int:
        max_tier = self.get_max_tier()
        return len([ts for ts in self.tiers if ts[1] == max_tier])

    @staticmethod
    def compare_lineups(lineup_1: PossibleLineup, lineup_2: PossibleLineup) -> int:
        lineup_1_value = Value(lineup_1)
        lineup_2_value = Value(lineup_2)

        tier_diff = lineup_1_value.get_max_tier() - lineup_2_value.get_max_tier()

        number_at_tier_diff = (
            lineup_1_value.get_number_at_max_tier()
            - lineup_2_value.get_number_at_max_tier()
        )
        # print(f"{(tier_diff, number_at_tier_diff)}")
        if tier_diff != 0:
            return tier_diff
        return number_at_tier_diff
