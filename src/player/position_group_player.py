"""A player in a position group"""
from dataclasses import dataclass, field
from src.team.team_or_multiple import TeamOrMultiple


@dataclass
class PositionGroupPlayer:
    """A player in a position group"""

    name: str
    teams: list[TeamOrMultiple]

    @staticmethod
    def from_dict(initial_dict: dict) -> "__class__":
        """Create from a dict"""
        return PositionGroupPlayer(
            name=initial_dict.get("name", "No Name Given"),
            teams=TeamOrMultiple.from_strings(initial_dict.get("teams", [])),
        )
