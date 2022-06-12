from dataclasses import dataclass, field
from typing import Set, MutableMapping

from aspy.Literal import BasicLiteral
from aspy.Symbol import Variable, Symbol


@dataclass(order=True)
class CoinductiveHypothesisSet:
    literals: Set[BasicLiteral] = field(default_factory=set)
    bindings: MutableMapping[Variable, Set[Symbol]] = field(default_factory=dict)

    @property
    def is_consistent(self) -> bool:
        for literal in self.literals:
            if -literal in self.literals:
                return False
        return True

    def add_literal(self, literal: BasicLiteral) -> bool:
        self.literals.add(literal)
        return -literal not in self.literals

    def __contains__(self, item):
        if isinstance(item, BasicLiteral):
            return item in self.literals
        return False

    def __str__(self):
        return self.fmt()

    def fmt(self, sep=' ', literal_sep=' '):
        fmt = "{"
        if self.literals:
            fmt += sep
        fmt += literal_sep.join(map(str, self.literals))
        if self.literals:
            fmt += sep
        fmt += "}"
        return fmt
