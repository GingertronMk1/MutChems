"""An enum class for positions"""
from enum import Enum
from dataclasses import dataclass

@dataclass
class NameAndAbbreviation:
    name: str
    abbreviation: str



class Position(Enum):
    """A list of positions"""

    QUARTERBACK = NameAndAbbreviation("Quarterback", "QB")
    HALFBACK = NameAndAbbreviation("Halfback", "HB")
    FULLBACK = NameAndAbbreviation("Fullback", "FB")
    WIDE_RECEIVER = NameAndAbbreviation("Wide Receiver", "WR")
    TIGHT_END = NameAndAbbreviation("Tight End", "TE")
    LEFT_TACKLE = NameAndAbbreviation("Left Tackle", "LT")
    LEFT_GUARD = NameAndAbbreviation("Left Guard", "LG")
    CENTER = NameAndAbbreviation("Center", "C")
    RIGHT_GUARD = NameAndAbbreviation("Right Guard", "RG")
    RIGHT_TACKLE = NameAndAbbreviation("Right Tackle", "RT")
    FREE_SAFETY = NameAndAbbreviation("Free Safety", "FS")
    STRONG_SAFETY = NameAndAbbreviation("Strong Safety", "SS")
    CORNERBACK = NameAndAbbreviation("Cornerback", "CB")
    RIGHT_OUTSIDE_LINEBACKER = NameAndAbbreviation("Right Outside Linebacker", "ROLB")
    MIDDLE_LINEBACKER = NameAndAbbreviation("Middle Linebacker", "MLB")
    LEFT_OUTSIDE_LINEBACKER = NameAndAbbreviation("Left Outside Linebacker", "LOLB")
    RIGHT_DEFENSIVE_END = NameAndAbbreviation("Right Defensive End", "RE")
    DEFENSIVE_TACKLE = NameAndAbbreviation("Defensive Tackle", "DT")
    LEFT_DEFENSIVE_END = NameAndAbbreviation("Left Defensive End", "LE")
    KICKER = NameAndAbbreviation("Kicker", "K")
    PUNTER = NameAndAbbreviation("Punter", "P")

    @staticmethod
    def get_from_name(name: str) -> "Position":
        for pos in list(Position):
            if pos.value.name == name:
                return pos
        raise ValueError(f"No position with name {name}")

    def get_from_abbreviation(abbreviation: str) -> "Position":
        for pos in list(Position):
            if pos.value.abbreviation == abbreviation:
                return pos
        raise ValueError(f"No position with abbreviation {abbreviation}")

