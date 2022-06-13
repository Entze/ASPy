from dataclasses import dataclass, field
from typing import Sequence, Optional, Dict, List, Tuple

from aspy.Atom import Atom
from aspy.ClauseElement import ClauseElement
from aspy.Comparison import Comparison, ComparisonOperator
from aspy.Literal import BasicLiteral
from aspy.Rule import Rule
from aspy.Symbol import Variable, Symbol, TopLevelSymbol


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
