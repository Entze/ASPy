from dataclasses import field, dataclass
from typing import Set, Mapping, Union, Sequence

from aspy.ClauseElement import HeadClauseElement
from aspy.Symbol import Variable, Symbol


@dataclass(order=True, frozen=True)
class Directive(HeadClauseElement):
    name: str
    arguments: Sequence[Union[Sequence[Symbol], Symbol]] = field(default_factory=tuple)

    @property
    def has_variable(self) -> bool:
        if self.name == 'forall':
            return True
        return any(argument.has_variable for argument in self.arguments)

    @property
    def variables(self) -> Set[Variable]:
        return set(variable for element in self.arguments for variable in element.variables)

    @property
    def is_true(self) -> bool:
        return self.name == 'true'

    @property
    def is_pos(self) -> bool:
        return self.is_true

    @property
    def is_false(self) -> bool:
        return self.name == 'false'

    @property
    def is_neg(self) -> bool:
        return self.is_false

    @property
    def is_forall(self) -> bool:
        return self.name == 'forall'

    @property
    def signature(self) -> str:
        return '#{}/{}.'.format(self.name, len(self.arguments))

    def __abs__(self):
        raise NotImplementedError

    def __neg__(self):
        if self.name == 'true':
            return Directive.false()
        elif self.name == 'false':
            return Directive.true()
        else:
            raise NotImplementedError

    def __str__(self):
        if not self.arguments:
            return "#{}".format(self.name)
        else:
            return "#{}({})".format(self.name, ','.join(map(str, self.arguments)))

    def substitute_variables(self, substitute_map: Mapping[Variable, Symbol]):
        arguments = tuple(argument.substitute_variables(substitute_map) for argument in self.arguments)
        return Directive(self.name, arguments)

    @staticmethod
    def forall(var: Sequence[Variable], goal: Union[Symbol]):
        return Directive('forall', (var, goal,))

    @staticmethod
    def true():
        return Directive('true')

    @staticmethod
    def false():
        return Directive('false')
