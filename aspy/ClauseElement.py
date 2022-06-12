import abc
from dataclasses import dataclass
from typing import Set, Mapping

from aspy.Symbol import Variable, Symbol


@dataclass(order=True, frozen=True)
class ClauseElement(abc.ABC):

    @property
    @abc.abstractmethod
    def has_variable(self) -> bool:
        raise NotImplementedError

    @property
    @abc.abstractmethod
    def variables(self) -> Set[Variable]:
        raise NotImplementedError

    @abc.abstractmethod
    def substitute_variables(self, substitute_map: Mapping[Variable, Symbol]):
        raise NotImplementedError

    def __neg__(self):
        raise NotImplementedError

    def __abs__(self):
        raise NotImplementedError


class HeadClauseElement(ClauseElement):
    pass
