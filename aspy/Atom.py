from dataclasses import dataclass, field
from typing import TypeVar, Set, Mapping

import clingo.ast

from aspy.Symbol import TopLevelSymbol, Function, Variable, Symbol

ForwardAtom = TypeVar('ForwardAtom', bound='Atom')


@dataclass(order=True, frozen=True)
class Atom:
    symbol: TopLevelSymbol = field(default_factory=Function)

    @property
    def has_variable(self) -> bool:
        return self.symbol.has_variable

    @property
    def variables(self) -> Set[Variable]:
        return self.symbol.variables

    @property
    def signature(self) -> str:
        return self.symbol.signature

    def match(self, other: ForwardAtom) -> bool:
        return self.symbol.function_name == other.symbol.function_name and \
               len(self.symbol.function_arguments) == len(other.symbol.function_arguments)

    def __str__(self):
        return str(self.symbol)

    def substitute_variables(self, substitute_map: Mapping[Variable, Symbol]):
        return Atom(self.symbol.substitute_variables(substitute_map))

