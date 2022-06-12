from collections import defaultdict
from dataclasses import dataclass, field
from functools import cached_property
from typing import Mapping, Sequence, List, Dict, Set

from aspy.Atom import Atom
from aspy.ClauseElement import ClauseElement, HeadClauseElement
from aspy.Directive import Directive
from aspy.Literal import Literal, BasicLiteral
from aspy.NormalRule import NormalRule
from aspy.Rule import Rule
from aspy.Symbol import Function, Term, IntegerConstant

RuleMap = Mapping[str, Mapping[str, Sequence[NormalRule]]]


@dataclass(order=True, frozen=True)
class Program:
    rules: Sequence[Rule] = field(default_factory=tuple)

    @cached_property
    def program_dicts(self):
        prop_atoms2rules = defaultdict(set)
        pred_atoms2rules = defaultdict(set)
        queue: List[Rule] = [*self.rules]
        while queue:
            rule = queue.pop(0)
            maybe_prop = not rule.has_variables
            signature = rule.head_signature
            maybe_prop = maybe_prop and signature not in pred_atoms2rules
            if maybe_prop:
                prop_atoms2rules[signature].add(rule)
            else:
                pred_atoms2rules[signature].add(rule)
        return prop_atoms2rules, pred_atoms2rules

    @cached_property
    def canonical_program_dicts(self):
        prop_atoms2rules = defaultdict(set)
        pred_atoms2rules = defaultdict(set)
        queue: List[Rule] = [*self.rules]
        while queue:
            rule = queue.pop(0)
            maybe_prop = not rule.has_variables
            signature = rule.head_signature
            maybe_prop = maybe_prop and signature not in pred_atoms2rules
            if maybe_prop:
                prop_atoms2rules[signature].add(rule)
            else:
                rule_vnf = rule.variable_normal_form()
                pred_atoms2rules[signature].add(rule_vnf)
                if signature in prop_atoms2rules:
                    queue.extend(prop_atoms2rules[signature])
                    prop_atoms2rules[signature].clear()
                for literal in rule.body:
                    if not literal.has_variable or not isinstance(literal, Literal):
                        continue
                    if abs(literal) in prop_atoms2rules:
                        queue.extend(prop_atoms2rules[literal.atom_signature])
                        prop_atoms2rules[literal.atom_signature].clear()
                    else:
                        pred_atoms2rules[literal.atom_signature] = set()

        return prop_atoms2rules, pred_atoms2rules

    @cached_property
    def sASP_program_dict(self):
        signature_rules = defaultdict(lambda: dict(primal=list(), dual=list()))
        prop_dual = Program.propositional_dual(self.canonical_propositional_rules)
        pred_dual = Program.predicate_dual(self.canonical_predicate_rules)
        prop, pred = self.program_dicts
        for signature, rules in prop.items():
            signature_rules[signature]['primal'].extend(rules)
        for signature, rules in pred.items():
            signature_rules[signature]['primal'].extend(rules)
        for rule in prop_dual:
            head_signature = rule.head_signature
            signature_rules[head_signature]['primal' if rule.head.is_pos else 'dual'].append(rule)
        for rule in pred_dual:
            head_signature = rule.head_signature
            signature_rules[head_signature]['primal' if rule.head.is_pos else 'dual'].append(rule)
        return signature_rules

    @property
    def propositional_rules(self) -> Sequence[Rule]:
        return tuple(rule for rules in self.program_dicts[0].values() for rule in rules)

    @property
    def predicate_rules(self) -> Sequence[Rule]:
        return tuple(rule for rules in self.program_dicts[1].values() for rule in rules)

    @property
    def canonical_propositional_rules(self) -> Sequence[Rule]:
        return tuple(rule for rules in self.canonical_program_dicts[0].values() for rule in rules)

    @property
    def canonical_predicate_rules(self) -> Sequence[Rule]:
        return tuple(rule for rules in self.canonical_program_dicts[1].values() for rule in rules)

    def fmt(self, sep=' ', begin=None, end=None):
        b = begin + sep if begin is not None else ''
        e = sep + end if end is not None else ''
        return "{}{}{}".format(b, sep.join(map(str, self.rules)), e)

    # def evaluate_backwards(self, *query: ClauseElement, nmr_check: bool = True):
    #     goal = Goal(query)
    #     root = GoalNode(subject=goal)
    #     program_dict = self.sASP_program_dict
    #     answer_set = 0
    #     work = [root]
    #     while work:
    #         current = work.pop()
    #         if current.is_success:
    #             if current.is_root:
    #                 yield deepcopy(current)
    #             else:
    #                 parent = current.propagate_to_parent()
    #                 work.append(parent)
    #         elif not current.is_exhausted:
    #             children = current.expand(program_dict)
    #             if children is None or not children:
    #                 work.append(current)
    #             else:
    #                 if isinstance(current, CallNode):
    #                     work.append(current)
    #                 work.extend(children)

    @staticmethod
    def propositional_dual(propositional_rules: Sequence[Rule]):
        dual_rules = []
        b2n: Dict[Sequence[ClauseElement], int] = {(): 0}
        n2b: Dict[int, Sequence[ClauseElement]] = {0: ()}
        n2h: Dict[int, Set[HeadClauseElement]] = {}
        h2n: Dict[HeadClauseElement, Set[int]] = {}
        ib: Set[Literal] = set()
        n = 0
        for rule in propositional_rules:
            assert isinstance(rule, NormalRule)
            head: BasicLiteral = rule.head
            body: Sequence[ClauseElement] = tuple(sorted(set(rule.body)))
            if body not in b2n:
                n += 1
                b2n[body] = n
                n2b[n] = body
            m = b2n[body]
            n2b[m] = body
            n2h.setdefault(m, set()).add(head)
            h2n.setdefault(head, set()).add(m)
        for h, ns in h2n.items():
            dual_head = -h
            dual_body = []
            dual_rule = None
            if ns and not any(n == 0 for n in ns):
                for n in ns:
                    b: Sequence[ClauseElement] = n2b[n]
                    if len(b) == 1:
                        if -b[0] not in dual_body:
                            dual_body.append(-b[0])
                        if isinstance(b[0], Literal):
                            ib.add(abs(b[0]))
                        dual_rule = NormalRule(dual_head, tuple(dual_body))
                    else:
                        assert len(b) > 1
                        if len(h2n[h]) == 1:
                            support_rule_head = -h
                            for l in b:
                                support_rule_body = (-l,)
                                support_rule = NormalRule(support_rule_head, support_rule_body)
                                dual_rules.append(support_rule)
                        else:
                            __b_n = BasicLiteral(atom=Atom(Function('__body', (Term(IntegerConstant(n)),))))
                            dual_body.append(-__b_n)
                            support_rule_head = -__b_n
                            for l in b:
                                if isinstance(l, Literal):
                                    ib.add(abs(l))
                                support_rule_body = (-l,)
                                support_rule = NormalRule(support_rule_head, support_rule_body)
                                dual_rules.append(support_rule)
                            dual_rule = NormalRule(dual_head, tuple(dual_body))

            if dual_rule is not None:
                dual_rules.append(dual_rule)
        for l in ib:
            if l not in h2n:
                dual_rules.append(NormalRule(-l))
        return dual_rules

    @staticmethod
    def predicate_dual(predicate_rules: Sequence[Rule]):
        dual_rules = []
        b2n: Dict[Sequence[ClauseElement], int] = {(): 0}
        n2b: Dict[int, Sequence[ClauseElement]] = {0: ()}
        n2h: Dict[int, Set[HeadClauseElement]] = {}
        h2n: Dict[HeadClauseElement, Set[int]] = {}
        ib: Set[ClauseElement] = set()
        n = 0
        for rule in predicate_rules:
            assert isinstance(rule, NormalRule)
            head: BasicLiteral = rule.head
            body: Sequence[ClauseElement] = tuple(set(rule.body))
            if body not in b2n:
                n += 1
                b2n[body] = n
                n2b[n] = body
            m = b2n[body]
            n2b[m] = body
            n2h.setdefault(m, set()).add(head)
            h2n.setdefault(head, set()).add(m)
        for h, ns in h2n.items():
            dual_head = -h
            dual_body = []
            dual_rule = None
            head_variables = h.variables

            if ns and not any(n == 0 for n in ns):
                for n in ns:
                    b: Sequence[ClauseElement] = n2b[n]
                    body_variables = set(variable for element in b for variable in element.variables)
                    existentials = body_variables - head_variables
                    func__bf_n_ = Function(name="__body_fails_", arguments=(
                        Term(IntegerConstant(n)),
                        Function(arguments=tuple(head_variables)),
                        Function(arguments=tuple(existentials))
                    ))
                    __bf_n_ = BasicLiteral(atom=Atom(func__bf_n_))
                    dual_body_literal = func__bf_n_
                    for existential in existentials:
                        dual_body_literal = Directive.forall(existential, dual_body_literal)
                    if not existentials:
                        dual_body_literal = __bf_n_
                    dual_body.append(dual_body_literal)

                    support_rule_head = __bf_n_
                    support_rules = []
                    for e in b:
                        support_rules.append(NormalRule(support_rule_head, (-e,)))

                    dual_rules.extend(support_rules)

                dual_rules.append(NormalRule(dual_head, dual_body))

            if dual_rule is not None:
                dual_rules.append(dual_rule)
        for l in ib:
            if l not in h2n:
                dual_rules.append(NormalRule(-l))
        return dual_rules
