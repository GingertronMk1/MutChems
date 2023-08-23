"""A team or multiple teams for chemistries"""
from src.team import Team


class TeamOrMultiple:
    """A team or multiple teams for chemistries"""

    name: Team
    number: int = 1
    children: list["__class__"]

    def __init__(self, name: Team, number: int = 1) -> None:
        self.name = name
        self.number = number

    @staticmethod
    def from_string(string: str) -> list["__class__"]:
        """Creating one from an encoded string"""
        if "|" in string:
            return [
                tom
                for subString in string.split("|")
                for tom in TeamOrMultiple.from_string(subString)
            ]
        if "." in string:
            tokens = string.split(".")
            return [TeamOrMultiple(Team(tokens[0]), tokens[1])]
        return [TeamOrMultiple(Team(string))]

    def __str__(self) -> str:
        return f"{self.name.value}.{self.number}"
