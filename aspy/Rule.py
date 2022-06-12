import abc
from dataclasses import dataclass, field
from typing import Optional, Sequence, Set

from aspy.ClauseElement import HeadClauseElement, ClauseElement
from aspy.Symbol import Variable


@dataclass(order=True, frozen=True)
class Rule(abc.ABC):
    head: Optional[HeadClauseElement] = field(default=None)
    body: Optional[Sequence[ClauseElement]] = field(default=None)

    @property
    def has_variables_in_head(self) -> bool:
        raise NotImplementedError

    @property
    def has_variables_in_body(self) -> bool:
        if self.body is not None:
            return any(element.has_variable for element in self.body)
        return False

    def variables_in_body(self) -> Set[Variable]:
        if self.body is not None:
            return set(variable for element in self.body for variable in element.variables)

    @property
    def has_variables(self) -> bool:
        return self.has_variables_in_head or self.has_variables_in_body

    @property
    @abc.abstractmethod
    def head_signature(self) -> str:
        raise NotImplementedError

    @abc.abstractmethod
    def variable_normal_form(self):
        raise NotImplementedError

    @staticmethod
    def fmt_body(body: Sequence[ClauseElement]):
        return ', '.join(map(str, body))
