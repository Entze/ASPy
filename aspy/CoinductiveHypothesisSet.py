from collections import defaultdict
from copy import deepcopy
from dataclasses import dataclass, field
from typing import Set, MutableMapping, Mapping, Sequence, TypeVar, Union

from aspy.Literal import BasicLiteral
from aspy.Symbol import Variable, Symbol, Term, Function, TopLevelSymbol

Unifications = Mapping[Variable, Set[Symbol]]
MutableUnifications = MutableMapping[Variable, Set[Symbol]]
NonVariable = Union[Term, Variable, Function]


def empty_unifications(**kwargs):
    return defaultdict(set, **kwargs)


ForwardCoinductiveHypothesisSet = TypeVar('ForwardCoinductiveHypothesisSet', bound='CoinductiveHypothesisSet')


@dataclass(order=True)
class CoinductiveHypothesisSet:
    literals: Set[BasicLiteral] = field(default_factory=set)
    bindings: MutableUnifications = field(default_factory=empty_unifications)
    prohibited: MutableUnifications = field(default_factory=empty_unifications)

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

    def fmt(self, sep=' ', literal_sep=' ', variable_sep=' '):
        fmt = "{"
        if self.literals:
            fmt += sep
        fmt += literal_sep.join(map(str, self.literals))
        if self.literals:
            fmt += sep
        fmt += "}" + sep + "{"
        if self.bindings:
            fmt += sep
        fmt += variable_sep.join(
            variable_sep.join("{} = {}".format(variable, binding) for binding in bindings) for variable, bindings in
            self.bindings.items())
        if self.bindings:
            fmt += sep
        fmt += "}" + sep + "{"
        if self.bindings:
            fmt += sep
        fmt += variable_sep.join(
            variable_sep.join("{} /= {}".format(variable, binding) for binding in bindings) for variable, bindings in
            self.prohibited.items())
        if self.bindings:
            fmt += sep
        fmt += "}"
        return fmt

    def unifies(self, left: Symbol, right: Symbol) -> bool:
        if left == right:
            return True
        elif isinstance(left, Variable) ^ isinstance(right, Variable):
            if isinstance(left, Variable) and isinstance(right, NonVariable):
                return self.__unifies_var_non_var(left, right)
            else:
                assert isinstance(right, Variable) and isinstance(left,
                                                                  NonVariable), "Unknown Symbol {} of type {}.".format(
                    left, type(left).__name__)
                return self.__unifies_var_non_var(right, left)
        elif isinstance(left, Variable) and isinstance(right, Variable):
            return self.__unifies_var_var(left, right)
        elif isinstance(left, TopLevelSymbol) and isinstance(right, TopLevelSymbol):
            return self.__unifies_compound_compound(left, right)
        else:
            return False

    def __unifies_var_non_var(self, var: Variable, non_var: NonVariable) -> bool:
        assert isinstance(var, Variable), "Symbol {} has to be a Variable, but is type {}.".format(var,
                                                                                                   type(var).__name__)
        assert isinstance(non_var, NonVariable), "Symbol {} has to be a NonVariable, but is type {}.".format(non_var,
                                                                                                             type(
                                                                                                                 non_var).__name__)
        prohibited_values = self.prohibited[var]
        return not any(self.unifies(prohibited_value, non_var) for prohibited_value in prohibited_values)

    def __unifies_var_var(self, var1: Variable, var2: Variable) -> bool:
        assert isinstance(var1, Variable), "Symbol {} has to be a Variable, but is type {}.".format(var1,
                                                                                                    type(var1).__name__)
        assert isinstance(var2, Variable), "Symbol {} has to be a Variable, but is type {}.".format(var2,
                                                                                                    type(var2).__name__)
        return True

    def __unifies_compound_compound(self, fun1: TopLevelSymbol, fun2: TopLevelSymbol) -> bool:
        assert isinstance(fun1, TopLevelSymbol), "Symbol {} has to be a TopLevelSymbol, but is type {}.".format(fun1,
                                                                                                                type(
                                                                                                                    fun1).__name__)
        assert isinstance(fun2, TopLevelSymbol), "Symbol {} has to be a TopLevelSymbol, but is type {}.".format(fun2,
                                                                                                                type(
                                                                                                                    fun2).__name__)
        return fun1.match(fun2) and all(
            self.unifies(fun1.function_arguments[i], fun2.function_arguments[i]) for i in range(fun1.arity))

    def constructive_unification(self, left: Symbol, right: Symbol) -> Sequence[ForwardCoinductiveHypothesisSet]:
        chs = deepcopy(self)
        return chs.constructive_unification_(left, right)

    def constructive_unification_(self, left: Symbol, right: Symbol) -> Sequence[ForwardCoinductiveHypothesisSet]:
        if left == right:
            return self,
        elif isinstance(left, Variable) ^ isinstance(right, Variable):
            if isinstance(left, Variable) and isinstance(right, NonVariable):
                return CoinductiveHypothesisSet.__constructive_unification_var_non_var(self, left, right)
            else:
                assert isinstance(right, Variable) and isinstance(left,
                                                                  NonVariable), "Unknown Symbol {} of type {}.".format(
                    left, type(left).__name__)
                return CoinductiveHypothesisSet.__constructive_unification_var_non_var(self, right, left)
        elif isinstance(left, Variable) and isinstance(right, Variable):
            return CoinductiveHypothesisSet.__constructive_unification_var_var(self, left, right)
        elif isinstance(left, TopLevelSymbol) and isinstance(right, TopLevelSymbol):
            return CoinductiveHypothesisSet.__constructive_unification_compound_compound(self, left, right)
        else:
            return ()

    @staticmethod
    def __constructive_unification_var_non_var(chs: ForwardCoinductiveHypothesisSet,
                                               var: Variable,
                                               non_var: NonVariable) -> Sequence[ForwardCoinductiveHypothesisSet]:
        assert isinstance(var, Variable), "Symbol {} has to be a Variable, but is type {}.".format(var,
                                                                                                   type(var).__name__)
        assert isinstance(non_var, NonVariable), "Symbol {} has to be a NonVariable, but is type {}.".format(non_var,
                                                                                                             type(
                                                                                                                 non_var).__name__)
        prohibited_values = chs.prohibited[var]
        if any(chs.unifies(prohibited_value, non_var) for prohibited_value in prohibited_values):
            return ()
        chs.bindings[var].add(non_var)
        return chs,

    @staticmethod
    def __constructive_unification_var_var(chs: ForwardCoinductiveHypothesisSet,
                                           var1: Variable,
                                           var2: Variable) -> Sequence[ForwardCoinductiveHypothesisSet]:
        assert isinstance(var1, Variable), "Symbol {} has to be a Variable, but is type {}.".format(var1,
                                                                                                    type(var1).__name__)
        assert isinstance(var2, Variable), "Symbol {} has to be a Variable, but is type {}.".format(var2,
                                                                                                    type(var2).__name__)
        chs.prohibited[var1].update(chs.prohibited[var2]), chs.prohibited[var2].update(chs.prohibited[var1])
        chs.bindings[var1].add(var2)
        chs.bindings[var2].add(var1)
        return chs,

    @staticmethod
    def __constructive_unification_compound_compound(chs: ForwardCoinductiveHypothesisSet,
                                                     fun1: TopLevelSymbol,
                                                     fun2: TopLevelSymbol) -> Sequence[ForwardCoinductiveHypothesisSet]:
        assert isinstance(fun1, TopLevelSymbol), "Symbol {} has to be a TopLevelSymbol, but is type {}.".format(fun1,
                                                                                                                type(
                                                                                                                    fun1).__name__)
        assert isinstance(fun2, TopLevelSymbol), "Symbol {} has to be a TopLevelSymbol, but is type {}.".format(fun2,
                                                                                                                type(
                                                                                                                    fun2).__name__)
        if not fun1.match(fun2):
            return ()
        unified = (chs,)
        for i in range(fun1.arity):
            if not unified:
                return ()
            chs, = unified
            unified = chs.constructive_unification_(fun1.function_arguments[i], fun2.function_arguments[i])
        return unified
