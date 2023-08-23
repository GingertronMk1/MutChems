"""An enum class for positions"""
from enum import Enum


class Position(Enum):
    """A list of positions"""

    QUARTERBACK = "Quarterback"
    HALFBACK = "Halfback"
    FULLBACK = "Fullback"
    WIDE_RECEIVER = "Wide Receiver"
    TIGHT_END = "Tight End"
    LEFT_TACKLE = "Left Tackle"
    LEFT_GUARD = "Left Guard"
    CENTER = "Center"
    RIGHT_GUARD = "Right Guard"
    RIGHT_TACKLE = "Right Tackle"
    FREE_SAFETY = "Free Safety"
    STRONG_SAFETY = "Strong Safety"
    CORNERBACK = "Cornerback"
    RIGHT_OUTSIDE_LINEBACKER = "Right Outside Linebacker"
    MIDDLE_LINEBACKER = "Middle Linebacker"
    LEFT_OUTSIDE_LINEBACKER = "Left Outside Linebacker"
    RIGHT_DEFENSIVE_END = "Right Defensive End"
    DEFENSIVE_TACKLE = "Defensive Tackle"
    LEFT_DEFENSIVE_END = "Left Defensive End"
    KICKER = "Kicker"
    PUNTER = "Punter"
