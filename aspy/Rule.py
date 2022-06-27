import abc
from dataclasses import dataclass, field
from typing import Optional, Sequence, Set, TypeVar, Dict, List, Tuple

import clingo.ast

from aspy.Atom import Atom
from aspy.ClauseElement import HeadClauseElement, ClauseElement
from aspy.Comparison import Comparison, ComparisonOperator
from aspy.Directive import Directive
from aspy.Literal import BasicLiteral
from aspy.Symbol import Variable, Symbol, TopLevelSymbol

ForwardRule = TypeVar('ForwardRule', bound='Rule')


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

    @property
    def variables_in_head(self) -> Set[Variable]:
        if self.head is None:
            return set()
        return self.head.variables

    @property
    def variables_in_body(self) -> Set[Variable]:
        if self.body is not None:
            return set(variable for element in self.body for variable in element.variables)
        return set()

    @property
    def has_variables(self) -> bool:
        return self.has_variables_in_head or self.has_variables_in_body

    @property
    @abc.abstractmethod
    def head_signature(self) -> str:
        raise NotImplementedError

    @property
    def variables(self) -> Set[Variable]:
        return self.variables_in_head | self.variables_in_body

    @abc.abstractmethod
    def variable_normal_form(self):
        raise NotImplementedError

    @staticmethod
    def fmt_body(body: Sequence[ClauseElement]):
        return ', '.join(map(str, body))


ForwardNormalRule = TypeVar('ForwardNormalRule', bound="NormalRule")

@dataclass(order=True, frozen=True)
class NormalRule(Rule):
    head: BasicLiteral = field(default_factory=BasicLiteral)
    body: Sequence[ClauseElement] = field(default_factory=tuple)

    @property
    def head_signature(self) -> str:
        return self.head.signature

    @property
    def has_variables_in_head(self) -> bool:
        return self.head.has_variable

    @property
    def has_existential_vars(self) -> bool:
        if not self.has_variables_in_body:
            return False
        return self.head.variables != set(variable for element in self.body for variable in element.variables)

    def __str__(self):
        if self.body:
            return "{} :- {}.".format(self.head, Rule.fmt_body(self.body))
        else:
            return "{}.".format(self.head)

    def variable_normal_form(self, env: Optional[Dict[tuple, Variable]] = None):
        if env is None:
            env = dict()
        self.head.atom.symbol.variable_normal_form_env(env)
        assert env is not None
        stack: List[Tuple[Symbol, tuple]] = []
        substitute_map = dict()
        for i, argument in enumerate(self.head.atom.symbol.function_arguments):
            stack.append((argument, (i,)))
        equalities = []
        new_head = BasicLiteral(self.head.sign, Atom(self.head.atom.symbol.variable_normal_form(env)))
        for i, argument in enumerate(new_head.atom.symbol.function_arguments):
            var = Variable("V{}".format(i))
            if var != argument:
                assert isinstance(argument, Variable)
                substitute_map[argument] = var
        while stack:
            current, pos = stack.pop(0)
            assert pos in env or isinstance(current, Variable), "Variable {} should have a pos {}.".format(current, pos)
            if isinstance(current, TopLevelSymbol):
                target = current.variable_normal_form(env, pos)
                for i, argument in enumerate(current.function_arguments):
                    stack.append((argument, (*pos, i)))
            else:
                target = current
            if env[pos] != target:
                equalities.append(
                    Comparison(env[pos], ComparisonOperator.Equal, target).substitute_variables(substitute_map))
        new_body = (*equalities, *(element.substitute_variables(substitute_map) for element in self.body))
        new_rule = NormalRule(new_head.substitute_variables(substitute_map), new_body)
        return new_rule




@dataclass(order=True, frozen=True)
class IntegrityConstraint(Rule):
    body: Sequence[ClauseElement] = field(default_factory=tuple)
    head: Directive = field(default_factory=Directive.false, init=False)

    @property
    def has_variables_in_head(self) -> bool:
        return False

    @property
    def head_signature(self) -> str:
        return "#false/0."

    def __str__(self):
        if self.body:
            return ":- {}.".format(Rule.fmt_body(self.body))
        else:
            return ":-."

    def variable_normal_form(self):
        return self


@dataclass(order=True, frozen=True)
class Goal(Rule):
    body: Sequence[ClauseElement] = field(default_factory=tuple)
    head: Directive = field(default=Directive.true(), init=False)

    @property
    def head_signature(self) -> str:
        return "#true/0."

    def __str__(self):
        if self.body:
            return '#true :- {}.'.format(Rule.fmt_body(self.body))
        else:
            return '#true.'

    def variable_normal_form(self):
        return self
