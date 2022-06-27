import abc
from dataclasses import dataclass, field
from functools import cached_property
from typing import TypeVar, Set, Mapping, Union, Sequence, Optional, List, Tuple, Dict

ForwardSymbol = TypeVar('ForwardSymbol', bound='Symbol')
ForwardVariable = TypeVar('ForwardVariable', bound='Variable')


class Symbol(abc.ABC):

    @property
    @abc.abstractmethod
    def has_variable(self) -> bool:
        raise NotImplementedError

    @property
    def variables(self) -> Set[ForwardVariable]:
        variables = set()
        stack = [self]
        while stack:
            current = stack.pop()
            if isinstance(current, Variable):
                variables.add(current)
            elif isinstance(current, TopLevelSymbol):
                stack.extend(current.function_arguments)
        return variables

    @abc.abstractmethod
    def substitute_variables(self, substitute_map: Mapping[ForwardVariable, ForwardSymbol]) -> ForwardSymbol:
        raise NotImplementedError


@dataclass(order=True, frozen=True)
class Variable(Symbol):
    name: str

    @property
    def has_variable(self) -> bool:
        return True

    def __str__(self):
        return self.name

    def substitute_variables(self, substitute_map: Mapping[ForwardVariable, ForwardSymbol]) -> Symbol:
        if self in substitute_map:
            return substitute_map[self]
        return self


@dataclass(order=True, frozen=True)
class StringConstant:
    string: str = field(default="")

    def __str__(self):
        return '"{}"'.format(self.string)


@dataclass(order=True, frozen=True)
class IntegerConstant:
    number: int = field(default=0)

    def __str__(self):
        return str(self.number)


ForwardTerm = TypeVar('ForwardTerm', bound='Term')


@dataclass(order=True, frozen=True)
class Term(Symbol):
    constant: Union[StringConstant, IntegerConstant] = field(default_factory=IntegerConstant)

    @property
    def has_variable(self) -> bool:
        return False

    def __str__(self):
        return str(self.constant)

    def substitute_variables(self, substitute_map: Mapping[ForwardVariable, ForwardSymbol]) -> Symbol:
        return self

    @staticmethod
    def zero():
        return Term(IntegerConstant(0))

    @staticmethod
    def one():
        return Term(IntegerConstant(1))


ForwardTopLevelSymbol = TypeVar('ForwardTopLevelSymbol', bound='TopLevelSymbol')


class TopLevelSymbol(Symbol, abc.ABC):

    @property
    def signature(self) -> str:
        return "{}/{}.".format(self.function_name, self.arity)

    @property
    @abc.abstractmethod
    def function_name(self) -> str:
        raise NotImplementedError

    @property
    def arity(self) -> int:
        return len(self.function_arguments)

    @property
    @abc.abstractmethod
    def function_arguments(self) -> Sequence[Symbol]:
        raise NotImplementedError

    def match(self, other: ForwardTopLevelSymbol) -> bool:
        return self.function_name == other.function_name and \
               len(self.function_arguments) == len(other.function_arguments)

    def variable_normal_form_env(self, env: Optional[Dict[tuple, Variable]] = None) -> Dict[tuple, Variable]:
        if env is None:
            env = dict()
        queue: List[Tuple[Symbol, tuple]] = [(self, ())]
        witnessed_variables = set()
        v = -1
        while queue:
            v += 1
            current, pos = queue.pop(0)
            if isinstance(current, Variable) and current not in witnessed_variables:
                witnessed_variables.add(current)
                env[pos] = current
                continue
            var = Variable("V{}".format(v))
            env[pos] = var
            if isinstance(current, TopLevelSymbol):
                for i, argument in enumerate(current.function_arguments):
                    queue.append((argument, (*pos, i)))
        return env

    @abc.abstractmethod
    def variable_normal_form(self, env: Dict[Symbol, Variable], context: tuple = ()):
        raise NotImplementedError

    @abc.abstractmethod
    def substitute_variables(self, substitute_map: Mapping[ForwardVariable, ForwardSymbol]) -> ForwardTopLevelSymbol:
        raise NotImplementedError


ForwardFunction = TypeVar('ForwardFunction', bound='Function')


@dataclass(order=True, frozen=True)
class Function(TopLevelSymbol):
    name: Optional[str] = field(default=None)
    arguments: Sequence[Symbol] = field(default_factory=tuple)

    @cached_property
    def has_variable(self) -> bool:
        return any(argument.has_variable for argument in self.arguments)

    @property
    def function_name(self) -> str:
        return self.name

    @property
    def function_arguments(self) -> Sequence[Symbol]:
        return self.arguments

    @property
    def arity(self) -> int:
        return len(self.arguments)

    def __str__(self):
        if self.name is None:
            return '({})'.format(','.join(map(str, self.arguments)))
        elif not self.arguments:
            return self.name
        else:
            return '{}({})'.format(self.name, ','.join(map(str, self.arguments)))

    def variable_normal_form(self, env: Dict[tuple, Variable], context: tuple = ()):
        arguments = []
        for i, argument in enumerate(self.arguments):
            pos = (*context, i)
            assert env[pos] is not None or isinstance(argument, Variable), "Variable {} should have pos {}.".format(
                argument, pos)
            if pos not in env:
                arguments.append(argument)
            else:
                arguments.append(env[pos])
        return Function(self.name, tuple(arguments))

    def substitute_variables(self, substitute_map: Mapping[Variable, Symbol]) -> TopLevelSymbol:
        arguments = []
        for argument in self.arguments:
            if isinstance(argument, Variable) and argument in substitute_map:
                arguments.append(substitute_map[argument])
            elif isinstance(argument, TopLevelSymbol):
                arguments.append(argument.substitute_variables(substitute_map))
            else:
                arguments.append(argument)
        return Function(self.name, tuple(arguments))
