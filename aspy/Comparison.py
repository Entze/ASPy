from dataclasses import dataclass, field
from enum import IntEnum
from typing import Set, Mapping, TypeVar

from aspy.ClauseElement import ClauseElement
from aspy.Symbol import Symbol, Variable

import clingo.ast

class ComparisonOperator(IntEnum):
    Equal = clingo.ast.ComparisonOperator.Equal
    GreaterEqual = clingo.ast.ComparisonOperator.GreaterEqual
    GreaterThan = clingo.ast.ComparisonOperator.GreaterThan
    LessEqual = clingo.ast.ComparisonOperator.LessEqual
    LessThan = clingo.ast.ComparisonOperator.LessThan
    NotEqual = clingo.ast.ComparisonOperator.NotEqual

    def __str__(self):
        if self is ComparisonOperator.Equal:
            op = '='
        elif self is ComparisonOperator.GreaterEqual:
            op = '>='
        elif self is ComparisonOperator.GreaterThan:
            op = '>'
        elif self is ComparisonOperator.LessEqual:
            op = '<='
        elif self is ComparisonOperator.LessThan:
            op = '<'
        else:
            assert self is ComparisonOperator.NotEqual, "Unknown ComparisonOperator {}".format(self)
            op = '!='
        return op

ForwardComparison = TypeVar('ForwardComparison', bound='Comparison')

@dataclass(order=True, frozen=True)
class Comparison(ClauseElement):
    left: Symbol = field(default_factory=Symbol)
    comparison: ComparisonOperator = field(default=ComparisonOperator.Equal)
    right: Symbol = field(default_factory=Symbol)

    def __abs__(self):
        raise NotImplementedError

    def __neg__(self):
        if self.comparison is ComparisonOperator.Equal:
            return Comparison(self.left, ComparisonOperator.NotEqual, self.right)
        elif self.comparison is ComparisonOperator.NotEqual:
            return Comparison(self.left, ComparisonOperator.Equal, self.right)
        else:
            raise NotImplementedError

    def __str__(self):
        return "{}{}{}".format(self.left, self.comparison, self.right)

    @property
    def has_variable(self) -> bool:
        return self.left.has_variable or self.right.has_variable

    @property
    def variables(self) -> Set[Variable]:
        return self.left.variables | self.right.variables

    @property
    def is_pos(self):
        return self.comparison is ComparisonOperator.Equal

    @property
    def is_neg(self):
        return self.comparison is not ComparisonOperator.Equal

    @property
    def signature(self) -> str:
        return "{}({},{})/2.".format(self.comparison, self.left, self.right)

    def substitute_variables(self, substitute_map: Mapping[Variable, Symbol]):
        return Comparison(self.left.substitute_variables(substitute_map), self.comparison,
                          self.right.substitute_variables(substitute_map))
