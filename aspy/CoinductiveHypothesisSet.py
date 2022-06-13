from collections import defaultdict
from copy import deepcopy
from dataclasses import dataclass, field
from typing import Set, MutableMapping, Mapping, Sequence, TypeVar, Union, ClassVar

from aspy.Literal import BasicLiteral
from aspy.NormalRule import NormalRule
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
    unused_var_int: ClassVar[int] = 0

    @property
    def is_consistent(self) -> bool:
        for literal in self.literals:
            if -literal in self.literals:
                return False
        return True

    @property
    def variables(self) -> Set[Variable]:
        return self.bindings.keys() | self.prohibited.keys() | self.literal_variables

    @property
    def literal_variables(self):
        return set(variable for literal in self.literals for variable in literal.variables)

    def add_literal(self, literal: BasicLiteral) -> bool:
        self.literals.add(literal)
        return -literal not in self.literals

    def __contains__(self, item):
        if isinstance(item, BasicLiteral):
            return item in self.literals
        return False

    def __str__(self):
        return self.fmt()

    def is_negatively_constrained(self, variable: Variable) -> bool:
        return variable in self.prohibited and self.prohibited[variable]

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
            variable_sep.join("{} = {}".format(variable, binding) for binding in bindings if bindings) for
            variable, bindings in
            self.bindings.items() if bindings)
        if self.bindings:
            fmt += sep
        fmt += "}" + sep + "{"
        if self.bindings:
            fmt += sep
        fmt += variable_sep.join(
            variable_sep.join("{} /= {}".format(variable, binding) for binding in bindings if bindings) for
            variable, bindings in
            self.prohibited.items() if bindings)
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

    def disunifies(self, left: Symbol, right: Symbol) -> bool:
        if left == right:
            return False
        elif isinstance(left, Variable) ^ isinstance(right, Variable):
            if isinstance(left, Variable) and isinstance(right, NonVariable):
                return self.__disunifies_var_non_var(left, right)
            else:
                assert isinstance(right, Variable) and isinstance(left,
                                                                  NonVariable), "Unknown Symbol {} of type {}.".format(
                    left, type(left).__name__)
                return self.__disunifies_var_non_var(right, left)
        elif isinstance(left, Variable) and isinstance(right, Variable):
            return self.__disunifies_var_var(left, right)
        elif isinstance(left, TopLevelSymbol) and isinstance(right, TopLevelSymbol):
            return self.__disunifies_compound_compound(left, right)
        else:
            return True

    def __disunifies_var_non_var(self, var: Variable, non_var: NonVariable) -> bool:
        assert isinstance(var, Variable), "Symbol {} has to be a Variable, but is type {}.".format(var,
                                                                                                   type(var).__name__)
        assert isinstance(non_var, NonVariable), "Symbol {} has to be a NonVariable, but is type {}.".format(non_var,
                                                                                                             type(
                                                                                                                 non_var).__name__)
        return True

    def __disunifies_var_var(self, var1: Variable, var2: Variable) -> bool:
        assert isinstance(var1, Variable), "Symbol {} has to be a Variable, but is type {}.".format(var1,
                                                                                                    type(var1).__name__)
        assert isinstance(var2, Variable), "Symbol {} has to be a Variable, but is type {}.".format(var2,
                                                                                                    type(var2).__name__)
        if self.is_negatively_constrained(var1) and self.is_negatively_constrained(var2):
            raise Exception("Disunification of two negatively constrained variables is not defined.")
        return self.bindings[var1].isdisjoint(self.bindings[var2])

    def __disunifies_compound_compound(self, fun1: TopLevelSymbol, fun2: TopLevelSymbol) -> bool:
        assert isinstance(fun1, TopLevelSymbol), "Symbol {} has to be a TopLevelSymbol, but is type {}.".format(fun1,
                                                                                                                type(
                                                                                                                    fun1).__name__)
        assert isinstance(fun2, TopLevelSymbol), "Symbol {} has to be a TopLevelSymbol, but is type {}.".format(fun2,
                                                                                                                type(
                                                                                                                    fun2).__name__)
        if not fun1.match(fun2):
            return True
        return any(self.disunifies(fun1.function_arguments[i], fun2.function_arguments[i]) for i in range(fun1.arity))

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

    def constructive_disunification(self, left: Symbol, right: Symbol) -> Sequence[ForwardCoinductiveHypothesisSet]:
        chs = deepcopy(self)
        return chs.constructive_disunification_(left, right)

    def constructive_disunification_(self, left: Symbol, right: Symbol) -> Sequence[ForwardCoinductiveHypothesisSet]:
        if left == right:
            return ()
        elif isinstance(left, Variable) ^ isinstance(right, Variable):
            if isinstance(left, Variable) and isinstance(right, NonVariable):
                return CoinductiveHypothesisSet.__constructive_disunification_var_non_var(self, left, right)
            else:
                assert isinstance(right, Variable) and isinstance(left,
                                                                  NonVariable), "Unknown Symbol {} of type {}.".format(
                    left, type(left).__name__)
                return CoinductiveHypothesisSet.__constructive_disunification_var_non_var(self, right, left)
        elif isinstance(left, Variable) and isinstance(right, Variable):
            return CoinductiveHypothesisSet.__constructive_disunification_var_var(self, left, right)
        elif isinstance(left, TopLevelSymbol) and isinstance(right, TopLevelSymbol):
            return CoinductiveHypothesisSet.__constructive_disunification_compound_compound(self, left, right)
        else:
            return self,

    def free(self, variable: Variable) -> ForwardCoinductiveHypothesisSet:
        chs = deepcopy(self)
        chs.free_(variable)
        return chs

    def free_(self, variable: Variable):
        self.bindings[variable].clear()
        self.prohibited[variable].clear()

    def get_free_var(self, prefix: str = "F") -> Variable:
        variable = Variable("{}{}".format(prefix, self.unused_var_int))
        self.unused_var_int += 1
        while variable in self.variables:
            variable = Variable("{}{}".format(prefix, self.unused_var_int))
            self.unused_var_int += 1
        return variable

    def rename(self, old_variable: Variable, new_variable: Variable) -> ForwardCoinductiveHypothesisSet:
        chs = deepcopy(self)
        chs.rename_(old_variable, new_variable)
        return chs

    def renames(self, rename_map: Mapping[Variable, Variable]) -> ForwardCoinductiveHypothesisSet:
        chs = deepcopy(self)
        chs.renames_(rename_map)
        return chs

    def rename_(self, old_variable: Variable, new_variable: Variable):
        literals_to_add = set()
        literals_to_remove = set()
        mapping = {old_variable: new_variable}
        for literal in self.literals:
            if old_variable in literal.variables:
                literals_to_remove.add(literal)
                literals_to_add.add(literal.substitute_variables(mapping))

        self.literals.difference_update(literals_to_remove)
        self.literals.update(literals_to_add)

        if old_variable in self.bindings:
            binds = self.bindings[old_variable]
            del self.bindings[old_variable]
            self.bindings[new_variable] = binds

        if old_variable in self.prohibited:
            prohibits = self.prohibited[old_variable]
            del self.prohibited[old_variable]
            self.prohibited[new_variable] = prohibits

    def renames_(self, rename_map: Mapping[Variable, Variable]):
        for old, new in rename_map.items():
            self.rename_(old, new)

    def propagate_literal_down_to_rule(self, literal: BasicLiteral,
                                       rule: NormalRule) -> ForwardCoinductiveHypothesisSet:
        chs = deepcopy(self)
        chs.propagate_literal_down_to_rule_(literal, rule)
        return chs

    def propagate_literal_down_to_rule_(self, literal: BasicLiteral, rule: NormalRule):
        to_rename = self.variables & rule.variables
        if to_rename:
            rename_map = {}
            for var in to_rename:
                rename_map[var] = self.get_free_var()
            self.renames_(rename_map)

        self.constructive_unification_(literal.atom.symbol, rule.head.atom.symbol)

    def propagate_rule_down_to_literal(self, rule: NormalRule,
                                       literal_index: int,
                                       literal: BasicLiteral) -> ForwardCoinductiveHypothesisSet:
        chs = deepcopy(self)
        chs.propagate_rule_down_to_literal_(rule, literal_index, literal)
        return chs

    def propagate_rule_down_to_literal_(self, rule: NormalRule, literal_index: int, literal: BasicLiteral):

        to_remove = set()
        for variable in self.bindings:
            if variable not in self.literal_variables and variable not in rule.body[literal_index].variables:
                to_remove.add(variable)
        for variable in to_remove:
            if variable in self.bindings:
                del self.bindings[variable]
            if variable in self.prohibited:
                del self.prohibited[variable]
        to_remove = set()
        for variable in self.prohibited:
            if variable not in self.literal_variables and variable not in rule.body[literal_index].variables:
                to_remove.add(variable)
        for variable in to_remove:
            if variable in self.prohibited:
                del self.prohibited[variable]

        rename_map = {}
        for elem in (*self.literals, rule.body[literal_index]):
            if not isinstance(elem, BasicLiteral):
                continue
            if elem.is_pos != literal.is_pos:
                continue
            if not elem.atom.match(literal.atom):
                continue
            if not self.unifies(elem.atom.symbol, literal.atom.symbol):
                continue
            queue = [(elem.atom.symbol, literal.atom.symbol)]
            while queue:
                old_arg, new_arg = queue.pop(0)
                if isinstance(old_arg, Variable) and isinstance(new_arg, Variable):
                    rename_map[old_arg] = new_arg
                elif isinstance(old_arg, TopLevelSymbol) and isinstance(new_arg, TopLevelSymbol):
                    if not old_arg.match(new_arg):
                        continue
                    for i in range(old_arg.arity):
                        queue.append((old_arg.function_arguments[i], new_arg.function_arguments[i]))
        self.renames_(rename_map)

    def propagate_literal_up_to_rule(self, literal: BasicLiteral, literal_index: int,
                                     rule: NormalRule) -> ForwardCoinductiveHypothesisSet:
        chs = deepcopy(self)
        chs.propagate_literal_up_to_rule_(literal, literal_index, rule)
        return chs

    def propagate_literal_up_to_rule_(self, literal: BasicLiteral, literal_index: int, rule: NormalRule):

        to_remove = set()
        for variable in self.bindings:
            if variable not in self.literal_variables and variable not in literal.variables:
                to_remove.add(variable)
        for variable in to_remove:
            if variable in self.bindings:
                del self.bindings[variable]
            if variable in self.prohibited:
                del self.prohibited[variable]
        to_remove = set()
        for variable in self.prohibited:
            if variable not in self.literal_variables and variable not in literal.variables:
                to_remove.add(variable)
        for variable in to_remove:
            if variable in self.prohibited:
                del self.prohibited[variable]

        body_literal = rule.body[literal_index]
        assert isinstance(body_literal, BasicLiteral)

        rename_map = {}
        for elem in (*self.literals, literal):
            if not isinstance(elem, BasicLiteral):
                continue
            if elem.is_pos != body_literal.is_pos:
                continue
            if not elem.atom.match(body_literal.atom):
                continue
            if not self.unifies(elem.atom.symbol, body_literal.atom.symbol):
                continue
            queue = [(elem.atom.symbol, body_literal.atom.symbol)]
            while queue:
                old_arg, new_arg = queue.pop(0)
                if isinstance(old_arg, Variable) and isinstance(new_arg, Variable):
                    rename_map[old_arg] = new_arg
                elif isinstance(old_arg, TopLevelSymbol) and isinstance(new_arg, TopLevelSymbol):
                    if not old_arg.match(new_arg):
                        continue
                    for i in range(old_arg.arity):
                        queue.append((old_arg.function_arguments[i], new_arg.function_arguments[i]))
        self.renames_(rename_map)

    def propagate_rule_up_to_literal(self, rule: NormalRule, literal: BasicLiteral) -> ForwardCoinductiveHypothesisSet:
        chs = deepcopy(self)
        chs.propagate_rule_up_to_literal_(rule, literal)
        return chs

    def propagate_rule_up_to_literal_(self, rule: NormalRule, literal: BasicLiteral):
        head_literal = rule.head
        rename_map = {}
        for elem in (*self.literals, head_literal):
            if not isinstance(elem, BasicLiteral):
                continue
            if elem.is_pos != literal.is_pos:
                continue
            if not elem.atom.match(literal.atom):
                continue
            if not self.unifies(elem.atom.symbol, literal.atom.symbol):
                continue
            queue = [(elem.atom.symbol, literal.atom.symbol)]
            while queue:
                old_arg, new_arg = queue.pop(0)
                if isinstance(old_arg, Variable) and isinstance(new_arg, Variable):
                    rename_map[old_arg] = new_arg
                elif isinstance(old_arg, TopLevelSymbol) and isinstance(new_arg, TopLevelSymbol):
                    if not old_arg.match(new_arg):
                        continue
                    for i in range(old_arg.arity):
                        queue.append((old_arg.function_arguments[i], new_arg.function_arguments[i]))
        self.renames_(rename_map)

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

    @staticmethod
    def __constructive_disunification_var_non_var(chs: ForwardCoinductiveHypothesisSet,
                                                  var: Variable,
                                                  non_var: NonVariable) -> Sequence[ForwardCoinductiveHypothesisSet]:
        assert isinstance(var, Variable), "Symbol {} has to be a Variable, but is type {}.".format(var,
                                                                                                   type(var).__name__)
        assert isinstance(non_var, NonVariable), "Symbol {} has to be a NonVariable, but is type {}.".format(non_var,
                                                                                                             type(
                                                                                                                 non_var).__name__)
        chs.prohibited[var].add(non_var)
        return chs,

    @staticmethod
    def __constructive_disunification_var_var(chs: ForwardCoinductiveHypothesisSet,
                                              var1: Variable,
                                              var2: Variable) -> Sequence[ForwardCoinductiveHypothesisSet]:
        assert isinstance(var1, Variable), "Symbol {} has to be a Variable, but is type {}.".format(var1,
                                                                                                    type(var1).__name__)
        assert isinstance(var2, Variable), "Symbol {} has to be a Variable, but is type {}.".format(var2,
                                                                                                    type(var2).__name__)
        if chs.is_negatively_constrained(var1) and chs.is_negatively_constrained(var2):
            raise Exception("Disunification of two negatively constrained variables is not defined.")

        chs.prohibited[var1].add(var2)
        chs.prohibited[var2].add(var1)
        return chs,

    @staticmethod
    def __constructive_disunification_compound_compound(chs: ForwardCoinductiveHypothesisSet,
                                                        fun1: TopLevelSymbol,
                                                        fun2: TopLevelSymbol) -> Sequence[
        ForwardCoinductiveHypothesisSet]:
        assert isinstance(fun1, TopLevelSymbol), "Symbol {} has to be a TopLevelSymbol, but is type {}.".format(fun1,
                                                                                                                type(
                                                                                                                    fun1).__name__)
        assert isinstance(fun2, TopLevelSymbol), "Symbol {} has to be a TopLevelSymbol, but is type {}.".format(fun2,
                                                                                                                type(
                                                                                                                    fun2).__name__)
        if not fun1.match(fun2):
            return (chs,)
        disunifications = []
        for i in range(fun1.arity):
            ds = chs.constructive_disunification(fun1.function_arguments[i], fun2.function_arguments[i])
            disunifications.extend(ds)
        return tuple(disunifications)
