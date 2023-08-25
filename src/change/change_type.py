from enum import Enum


class ChangeType(Enum):
    REMOVALS = "removals"
    ADDITIONS = "additions"
    REPLACEMENT = "replacement"
    NOCHANGE = "no_change"
