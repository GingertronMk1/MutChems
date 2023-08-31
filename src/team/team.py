"""A list of teams in the NFL for chemistry purposes"""
from enum import Enum


class Team(Enum):
    """A list of teams in the NFL for chemistry purposes"""

    NO_TEAM = "No Team"
    BEARS = "Bears"
    BENGALS = "Bengals"
    BILLS = "Bills"
    BRONCOS = "Broncos"
    BROWNS = "Browns"
    BUCCANEERS = "Buccaneers"
    CARDINALS = "Cardinals"
    CHARGERS = "Chargers"
    CHIEFS = "Chiefs"
    COLTS = "Colts"
    COMMANDERS = "Commanders"
    COWBOYS = "Cowboys"
    DOLPHINS = "Dolphins"
    EAGLES = "Eagles"
    FALCONS = "Falcons"
    GIANTS = "Giants"
    JAGUARS = "Jaguars"
    JETS = "Jets"
    LIONS = "Lions"
    NINERS = "49ers"
    PACKERS = "Packers"
    PANTHERS = "Panthers"
    PATRIOTS = "Patriots"
    RAIDERS = "Raiders"
    RAMS = "Rams"
    RAVENS = "Ravens"
    SAINTS = "Saints"
    SEAHAWKS = "Seahawks"
    STEELERS = "Steelers"
    TEXANS = "Texans"
    TITANS = "Titans"
    VIKINGS = "Vikings"
    ALL32 = "All32Teams"

    @staticmethod
    def all_teams() -> list["Team"]:
        """All the actual teams"""
        return [t for t in list(Team) if t not in Team.not_real_teams()]

    @staticmethod
    def not_real_teams() -> list["Team"]:
        """All the not-real teams, so all32 and no team (for now)"""
        return [Team.ALL32, Team.NO_TEAM]
