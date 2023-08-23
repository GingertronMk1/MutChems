from enum import Enum


class Position(Enum):
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

    @staticmethod
    def offense() -> list["__class__"]:
        return [
            Position.QUARTERBACK,
            Position.HALFBACK,
            Position.FULLBACK,
            Position.WIDE_RECEIVER,
            Position.TIGHT_END,
            Position.LEFT_TACKLE,
            Position.LEFT_GUARD,
            Position.CENTER,
            Position.RIGHT_GUARD,
            Position.RIGHT_TACKLE,
        ]

    @staticmethod
    def defense() -> list["__class__"]:
        return [
            Position.FREE_SAFETY,
            Position.STRONG_SAFETY,
            Position.CORNERBACK,
            Position.RIGHT_OUTSIDE_LINEBACKER,
            Position.MIDDLE_LINEBACKER,
            Position.LEFT_OUTSIDE_LINEBACKER,
            Position.RIGHT_DEFENSIVE_END,
            Position.DEFENSIVE_TACKLE,
            Position.LEFT_DEFENSIVE_END,
        ]

    @staticmethod
    def specialTeams() -> list["__class__"]:
        return [Position.KICKER, Position.PUNTER]
