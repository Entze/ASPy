from copy import deepcopy
from dataclasses import dataclass, field
from typing import Union, TypeVar, Optional, Sequence, MutableSequence

from aspy.ClauseElement import ClauseElement
from aspy.CoinductiveHypothesisSet import CoinductiveHypothesisSet
from aspy.Directive import Directive
from aspy.Goal import Goal
from aspy.Literal import BasicLiteral
from aspy.NormalRule import NormalRule
from aspy.Program import RuleMap

ForwardBaseNode = TypeVar('ForwardBaseNode', bound='BaseNode')


@dataclass(order=True)
class BaseNode:
    subject: Union[None, ClauseElement, NormalRule]
    parent: Optional[ForwardBaseNode]
    children: Optional[MutableSequence[ForwardBaseNode]]
    hypotheses: Optional[MutableSequence[CoinductiveHypothesisSet]]

    @property
    def is_success(self) -> bool:
        raise NotImplementedError

    @property
    def is_expanded(self) -> bool:
        return self.children is not None

    @property
    def is_root(self) -> bool:
        return self.parent is None

    def __str__(self):
        if self.is_expanded:
            return "{} {} [{}]".format(self.subject, self.is_success, ', '.join(map(str, self.hypotheses)))
        else:
            return "({}) [{}]".format(self.subject, ', '.join(map(str, self.hypotheses)))

    def __eq__(self, other):
        if isinstance(other, BaseNode):
            return self.__eq_level(other) and self.__eq_up(other) and self.__eq_down(other)
        return False

    def __eq_level(self, other: ForwardBaseNode):
        if self.is_root != other.is_root:
            return False
        if self.is_expanded != other.is_expanded:
            return False
        if self.subject != other.subject:
            return False
        if self.hypotheses != other.hypotheses:
            return False
        if len(self.children) != len(self.children):
            return False
        if self.is_success != other.is_success:
            return False
        return True

    def __eq_up(self, other: ForwardBaseNode):
        if self.is_root:
            return other.is_root
        if not self.parent.__eq_level(other.parent):
            return False
        if not self.parent.__eq_up(other.parent):
            return False
        return True

    def __eq_down(self, other: ForwardBaseNode):
        if not self.is_expanded:
            return not other.is_expanded
        for i, child in enumerate(self.children):
            if not child.__eq_level(other.children[i]):
                return False
        for i, child in enumerate(self.children):
            if not child.__eq_down(other.children[i]):
                return False
        return True

    def expand(self, rule_map: Optional[RuleMap] = None):
        raise NotImplementedError

    def init(self):
        self.hypotheses = [CoinductiveHypothesisSet()]


@dataclass(order=True)
class Node(BaseNode):
    subject: Union[None, ClauseElement, NormalRule] = field(default=None)
    parent: Optional[BaseNode] = field(default=None, repr=False, compare=False)
    children: Optional[MutableSequence[BaseNode]] = field(default=None, repr=False, compare=False)
    hypotheses: Optional[MutableSequence[CoinductiveHypothesisSet]] = field(default=None)

    @property
    def is_success(self) -> bool:
        raise NotImplementedError

    def expand(self, rule_map: Optional[RuleMap] = None):
        raise NotImplementedError


@dataclass(order=True)
class Leaf(Node):
    subject: Directive = field(default_factory=Directive.false)

    @property
    def is_success(self) -> bool:
        return self.subject.is_true

    @property
    def is_expanded(self) -> bool:
        return True

    def __eq__(self, other):
        if isinstance(other, Leaf):
            return self.is_success != other.is_success

    def __str__(self):
        if self.is_success:
            return u"⊤"
        elif self.subject.is_false:
            return u"⊥"
        return repr(self)

    def expand(self, rule_map: Optional[RuleMap] = None):
        pass

    @staticmethod
    def success(parent: Optional[BaseNode] = None):
        return Leaf(subject=Directive.true(), parent=parent)

    @staticmethod
    def fail(parent: Optional[BaseNode] = None):
        return Leaf(subject=Directive.false(), parent=parent)


@dataclass(order=True)
class RuleNode(Node):
    subject: NormalRule = field(default_factory=NormalRule)

    @property
    def is_success(self) -> bool:
        return self.is_expanded and all(child.is_success for child in self.children)

    def expand(self, rule_map: Optional[RuleMap] = None):
        if self.is_expanded:
            return
        self.children = []
        for literal_index,clause_element in enumerate(self.subject.body):
            if isinstance(clause_element, BasicLiteral):
                for hypothesis in self.hypotheses:
                    hypothesis.propagate_rule_down_to_literal_(self.subject, literal_index, clause_element)
                child = LiteralNode(subject=clause_element,
                                    parent=self,
                                    hypotheses=self.hypotheses)
                child.expand(rule_map)
                self.children.append(child)
                for hypothesis in child.hypotheses:
                    hypothesis.propagate_literal_up_to_rule_(child.subject, literal_index, self.subject)
                self.hypotheses = child.hypotheses
            else:
                assert False, "Unexpected Node {} with type {}.".format(clause_element, type(clause_element).__name__)
        if not self.subject.body:
            child = Leaf.success(self)
            self.children.append(child)


@dataclass(order=True)
class GoalNode(RuleNode):
    subject: Goal = field(default_factory=Goal)

    def answer_sets(self) -> Sequence[CoinductiveHypothesisSet]:
        return tuple(hypothesis for hypothesis in self.hypotheses if hypothesis.is_consistent)


@dataclass(order=True)
class LiteralNode(Node):
    subject: BasicLiteral = field(default_factory=BasicLiteral)

    @property
    def is_success(self) -> bool:
        return self.is_expanded and any(child.is_success for child in self.children)

    def expand(self, rule_map: Optional[RuleMap] = None):
        if self.is_expanded:
            return
        self.children = []

        conclusive = self.coinductive_hypotheses_check()

        if not conclusive:
            rules = rule_map[self.subject.signature]['primal' if self.subject.is_pos else 'dual']
            expansion_impossible = True
            for rule in rules:
                if any(hypothesis.unifies(self.subject.atom.symbol, rule.head.atom.symbol) for hypothesis in self.hypotheses):
                    hypotheses = [hypothesis.propagate_literal_down_to_rule(self.subject, rule) for hypothesis in self.hypotheses]
                    child = RuleNode(subject=rule,
                                     parent=self,
                                     hypotheses=hypotheses)
                    self.children.append(child)
                    expansion_impossible = False
            if expansion_impossible:
                child = Leaf.fail(self)
                self.children.append(child)
            for child in self.children:
                child.expand(rule_map)

        hypotheses = []
        for child in self.children:
            if child.is_success and not isinstance(child, Leaf):
                for hypothesis in child.hypotheses:
                    hypothesis.propagate_rule_up_to_literal_(child.subject, self.subject)
                    if hypothesis not in hypotheses:
                        hypotheses.append(hypothesis)
        if self.children and all(child.is_success for child in self.children) and not hypotheses:
            hypotheses = [CoinductiveHypothesisSet()]
        self.hypotheses = hypotheses

        for hypothesis in self.hypotheses:
            hypothesis.add_literal(self.subject)

    def coinductive_hypotheses_check(self):
        if self.hypotheses and all(self.subject in hypothesis for hypothesis in self.hypotheses):
            child = Leaf.success(self)
            self.children.append(child)
            return True
        if self.hypotheses and all(-self.subject in hypothesis for hypothesis in self.hypotheses):
            child = Leaf.fail(self)
            self.children.append(child)
            return True

        negations = 0
        node: LiteralNode = self
        while not node.parent.is_root:
            node: LiteralNode = node.parent.parent
            if node.subject == self.subject:
                if not self.subject.is_pos or negations > 0:
                    child = Leaf.success(self)
                else:
                    child = Leaf.fail(self)
                self.children.append(child)
                return True
            if node.subject.is_neg:
                negations += 1
        return False
