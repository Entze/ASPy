import abc
from dataclasses import dataclass, field
from enum import IntEnum
from typing import Set, Mapping, Optional, Union

from aspy.Atom import Atom
from aspy.ClauseElement import HeadClauseElement
from aspy.Symbol import Variable, Symbol, Function, IntegerConstant, Term


class Literal(HeadClauseElement):

    @property
    @abc.abstractmethod
    def atom_signature(self) -> str:
        raise NotImplementedError

    @property
    @abc.abstractmethod
    def is_pos(self) -> bool:
        raise NotImplementedError

    @property
    @abc.abstractmethod
    def is_neg(self) -> bool:
        raise NotImplementedError


class Sign(IntEnum):
    NoSign = 0
    Negation = 1

    def __str__(self):
        if self is Sign.NoSign:
            return ''
        elif self is Sign.Negation:
            return 'not'
        else:
            assert False, 'Unknown IntEnum {} = {}.'.format(self.name, self.value)


@dataclass(order=True, frozen=True)
class BasicLiteral(Literal):
    sign: Sign = Sign.NoSign
    atom: Atom = field(default_factory=Atom)

    @property
    def is_pos(self) -> bool:
        return self.sign is Sign.NoSign

    @property
    def is_neg(self) -> bool:
        return self.sign is Sign.Negation

    @property
    def has_variable(self) -> bool:
        return self.atom.has_variable

    @property
    def variables(self) -> Set[Variable]:
        return self.atom.variables

    @property
    def atom_signature(self) -> str:
        return self.atom.signature

    def __str__(self):
        if self.sign is Sign.NoSign:
            return "{}".format(self.atom)
        else:
            return "{} {}".format(self.sign, self.atom)

    def __neg__(self):
        return BasicLiteral(Sign((self.sign ^ 1) % 2), self.atom)

    def __abs__(self):
        return BasicLiteral(Sign.NoSign, self.atom)

    def substitute_variables(self, substitute_map: Mapping[Variable, Symbol]):
        return BasicLiteral(self.sign, self.atom.substitute_variables(substitute_map))

    @staticmethod
    def make_literal(name: Optional[str], *arguments: Union[str, int, Variable, Function]):
        function_arguments = []
        for arg_ in arguments:
            if isinstance(arg_, str):
                arg = Function(arg_)
            elif isinstance(arg_, int):
                arg = Term(IntegerConstant(arg_))
            else:
                arg = arg_
            function_arguments.append(arg)
        return BasicLiteral(atom=Atom(Function(name=name, arguments=tuple(function_arguments))))
