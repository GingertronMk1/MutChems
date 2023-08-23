from src.team import Team


class TeamOrMultiple:
    name: Team
    number: int = 1
    children: list["__class__"]

    def __init__(self, name: Team, number: int = 1) -> None:
        self.name = name
        self.number = number

    @staticmethod
    def fromString(string: str) -> list["__class__"]:
        if "|" in string:
            return [
                tom
                for subString in string.split("|")
                for tom in TeamOrMultiple.fromString(subString)
            ]
        if "." in string:
            tokens = string.split(".")
            return [TeamOrMultiple(Team(tokens[0]), tokens[1])]
        return [TeamOrMultiple(Team(string))]

    def __str__(self) -> str:
        return "{team}.{number}".format(team=self.name.value, number=self.number)
