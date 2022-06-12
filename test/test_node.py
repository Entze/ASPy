import unittest
from typing import cast

from aspy.CoinductiveHypothesisSet import CoinductiveHypothesisSet
from aspy.Goal import Goal
from aspy.Literal import BasicLiteral
from aspy.Node import RuleNode, LiteralNode, Leaf, GoalNode
from aspy.NormalRule import NormalRule
from aspy.Program import RuleMap

p = BasicLiteral.make_literal('p')
q = BasicLiteral.make_literal('q')
r = BasicLiteral.make_literal('r')
s = BasicLiteral.make_literal('s')
t = BasicLiteral.make_literal('t')


# noinspection DuplicatedCode
class TestNodeMethods(unittest.TestCase):

    def test_prop_failing_branching_expand_literal_node(self):
        """
        Test if a node

        (p :- q, r.) [{}]

        with the KB:

        p :- q, r.
        not p :- not q.
        not p :- not r.
        not q.
        not r.

        expands correctly.

        :return:
        """
        r1 = NormalRule(p, (q, r))
        d1 = NormalRule(-p, (-q))
        d2 = NormalRule(-p, (-r))
        d3 = NormalRule(-q)
        d4 = NormalRule(-r)
        rule_map: RuleMap = {
            "p/0.": {"primal": [r1], "dual": [d1, d2]},
            "q/0.": {"primal": [], "dual": [d3]},
            "r/0.": {"primal": [], "dual": [d4]},
        }

        actual = RuleNode(subject=r1)
        actual.init()

        actual.expand(rule_map)

        expected = RuleNode(subject=r1, hypotheses=[])
        child_q = LiteralNode(subject=q,
                              parent=expected,
                              hypotheses=[])
        fail_q = Leaf.fail(child_q)
        child_q.children = [fail_q]
        child_r = LiteralNode(subject=r,
                              parent=expected,
                              hypotheses=[])
        fail_r = Leaf.fail(child_r)
        child_r.children = [fail_r]
        expected.children = [child_q, child_r]
        self.assertEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}".format(expected, actual))

    def test_prop_succeeding_branching_expand_literal_node(self):
        """
        Test if a node

        (p :- q, r.) [{}]

        with the KB:

        p :- q, r.
        q. r.
        not p :- not q.
        not p :- not r.

        expands correctly.

        :return:
        """
        r1 = NormalRule(p, (q, r))
        r2 = NormalRule(q)
        r3 = NormalRule(r)
        d1 = NormalRule(-p, (-q))
        d2 = NormalRule(-p, (-r))
        rule_map: RuleMap = {
            "p/0.": {"primal": [r1], "dual": [d1, d2]},
            "q/0.": {"primal": [r2], "dual": []},
            "r/0.": {"primal": [r3], "dual": []},
        }

        actual = RuleNode(subject=r1)
        actual.init()

        actual.expand(rule_map)

        expected = RuleNode(subject=r1,
                            hypotheses=[CoinductiveHypothesisSet({q, r})])

        child_lit_q = LiteralNode(subject=q,
                                  parent=expected,
                                  hypotheses=[CoinductiveHypothesisSet({q})])

        child_rule_q = RuleNode(subject=r2,
                                parent=child_lit_q,
                                hypotheses=[CoinductiveHypothesisSet()])
        child_lit_q.children = [child_rule_q]
        child_success_q = Leaf.success(child_rule_q)
        child_rule_q.children = [child_success_q]
        child_lit_r = LiteralNode(subject=r,
                                  parent=expected,
                                  hypotheses=[CoinductiveHypothesisSet({q, r})])
        child_rule_r = LiteralNode(subject=r,
                                   parent=child_lit_r,
                                   hypotheses=[CoinductiveHypothesisSet({q})])
        child_lit_r.children = [child_rule_r]
        success_r = Leaf.fail(child_rule_r)
        child_rule_r.children = [success_r]
        expected.children = [child_lit_q, child_lit_r]
        self.assertEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}".format(expected, actual))

    def test_prop_tight_positive_branching(self):
        """
        Test if the query

        #true :- p.

        with the KB:

        p :- q.
        p :- r.
        q. r.
        not p :- not q, not r.

        expands correctly and has the correct answer sets.

        :return:
        """
        r1 = NormalRule(p, (q,))
        r2 = NormalRule(p, (r,))
        r3 = NormalRule(q)
        r4 = NormalRule(r)
        d1 = NormalRule(-p, (-q, -r))
        rule_map: RuleMap = {
            "p/0.": {"primal": [r1, r2], "dual": [d1]},
            "q/0.": {"primal": [r3], "dual": []},
            "r/0.": {"primal": [r4], "dual": []},
        }

        goal = Goal((p,))

        actual = GoalNode(subject=goal)
        actual.init()
        actual.expand(rule_map)

        expected = GoalNode(subject=goal,
                            hypotheses=[CoinductiveHypothesisSet({p, q}), CoinductiveHypothesisSet({p, r})])

        child_lit_p = RuleNode(subject=p,
                               parent=expected,
                               hypotheses=[CoinductiveHypothesisSet({p, q}), CoinductiveHypothesisSet({p, r})])
        expected.children = [child_lit_p]

        child_rule_r1 = RuleNode(subject=r1,
                                 parent=child_lit_p,
                                 hypotheses=[CoinductiveHypothesisSet({q})])
        child_rule_r2 = RuleNode(subject=r2,
                                 parent=child_lit_p,
                                 hypotheses=[CoinductiveHypothesisSet({r})])
        child_lit_p.children = [child_rule_r1, child_rule_r2]

        child_lit_q = LiteralNode(subject=q,
                                  parent=child_rule_r1,
                                  hypotheses=[CoinductiveHypothesisSet({q})])
        child_rule_r1.children = [child_lit_q]
        child_rule_q = RuleNode(subject=r2,
                                parent=child_lit_q,
                                hypotheses=[CoinductiveHypothesisSet()])
        child_lit_q.children = [child_rule_q]
        child_success_q = Leaf.success(child_rule_q)
        child_rule_q.children = [child_success_q]

        child_lit_r = LiteralNode(subject=r,
                                  parent=child_rule_r2,
                                  hypotheses=[CoinductiveHypothesisSet({r})])
        child_rule_r2.children = [child_lit_r]
        child_rule_r = LiteralNode(subject=r,
                                   parent=child_lit_r,
                                   hypotheses=[CoinductiveHypothesisSet()])
        child_lit_r.children = [child_rule_r]
        success_r = Leaf.fail(child_rule_r)
        child_rule_r.children = [success_r]
        expected.children = [child_lit_q, child_lit_r]
        self.assertEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}".format(expected, actual))
        expected_answer_sets = (CoinductiveHypothesisSet({p, q}), CoinductiveHypothesisSet({p, r}))
        actual_answer_sets = cast(tuple, actual.answer_sets())
        self.assertTupleEqual(expected_answer_sets, actual_answer_sets)

    def test_prop_simple_negative(self):
        """
        Test if the query

        #true :- not p.

        with the KB:

        p :- q.
        not p :- not q.
        not q.

        expands correctly and has the correct answer sets

        :return:
        """
        r1 = NormalRule(p, (q,))
        d1 = NormalRule(-p, (-q,))
        d2 = NormalRule(-q)
        rule_map: RuleMap = {
            "p/0.": {"primal": [r1], "dual": [d1]},
            "q/0.": {"primal": [], "dual": [d2]},
        }

        goal = Goal((-p,))

        actual = GoalNode(goal)
        actual.init()
        actual.expand(rule_map)

        expected = GoalNode(subject=goal,
                            hypotheses=[CoinductiveHypothesisSet({-p, -q})])

        child_lit_not_p = LiteralNode(subject=-p,
                                      parent=expected,
                                      hypotheses=[CoinductiveHypothesisSet({-p, -q})])
        expected.children = [child_lit_not_p]
        child_rule_d1 = RuleNode(subject=d1,
                                 parent=child_lit_not_p,
                                 hypotheses=[CoinductiveHypothesisSet({-p, -q})])
        child_lit_not_p.children = [child_rule_d1]

        child_lit_not_q = LiteralNode(subject=-q,
                                      parent=child_rule_d1,
                                      hypotheses=[CoinductiveHypothesisSet({-p, -q})])

        child_rule_d1.children = [child_lit_not_q]

        child_rule_d2 = RuleNode(subject=d2,
                                 parent=child_lit_not_q,
                                 hypotheses=[CoinductiveHypothesisSet({-p, -q})])
        child_lit_not_q.children = [child_rule_d2]

        child_success = Leaf.success(child_lit_not_q)
        child_lit_not_q.children = [child_success]

        self.assertEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}\n".format(expected, actual))

        expected_answer_sets = (CoinductiveHypothesisSet({-p, -q}),)
        actual_answer_sets = cast(tuple, actual.answer_sets())
        self.assertTupleEqual(expected_answer_sets, actual_answer_sets)

    def test_prop_loop_negative_1(self):
        """
        Test if the query

        #true :- p.

        with the KB:

        p :- not q.
        q :- not p.
        not p :- q.
        not q :- p.

        has the correct answer sets.

        :return:
        """
        r1 = NormalRule(p, (-q,))
        r2 = NormalRule(-q, (-p,))
        d1 = NormalRule(-p, (q,))
        d2 = NormalRule(-q, (p,))
        rule_map: RuleMap = {
            "p/0.": {"primal": [r1], "dual": [d1]},
            "q/0.": {"primal": [r2], "dual": [d2]},
        }

        goal = Goal((p,))
        node = GoalNode(subject=goal)
        node.init()
        node.expand(rule_map)

        actual = cast(tuple, node.answer_sets())
        expected = (CoinductiveHypothesisSet({p, -q}),)
        self.assertTupleEqual(expected, actual,
                              msg="\nExpected: ({})\n  Actual: ({})\n".format(",".join(map(str, expected)),
                                                                              ",".join(map(str, actual))))

    def test_prop_loop_negative_2(self):
        """
        Test if the query

        #true :- not p.

        with the KB:

        p :- not q.
        q :- not p.
        not p :- q.
        not q :- p.

        has the correct answer sets.

        :return:
        """
        r1 = NormalRule(p, (-q,))
        r2 = NormalRule(q, (-p,))
        d1 = NormalRule(-p, (q,))
        d2 = NormalRule(-q, (p,))
        rule_map: RuleMap = {
            "p/0.": {"primal": [r1], "dual": [d1]},
            "q/0.": {"primal": [r2], "dual": [d2]},
        }

        goal = Goal((-p,))
        node = GoalNode(subject=goal)
        node.init()
        node.expand(rule_map)

        actual = cast(tuple, node.answer_sets())
        expected = (CoinductiveHypothesisSet({-p, q}),)
        self.assertTupleEqual(expected, actual,
                              msg="\nExpected: ({})\n  Actual: ({})\n".format(",".join(map(str, expected)),
                                                                              ",".join(map(str, actual))))

    def test_prop_loop_positive(self):
        """
        Test if the query

        #true :- p.

        with the KB:

        p :- q.
        q :- p.
        not p :- not q.
        not q :- not p.

        has the correct answer sets.

        :return:
        """
        r1 = NormalRule(p, (q,))
        r2 = NormalRule(q, (p,))
        d1 = NormalRule(-p, (-q,))
        d2 = NormalRule(-q, (-p,))
        rule_map: RuleMap = {
            "p/0.": {"primal": [r1], "dual": [d1]},
            "q/0.": {"primal": [r2], "dual": [d2]},
        }

        goal = Goal((p,))
        node = GoalNode(subject=goal)
        node.init()
        node.expand(rule_map)

        actual = cast(tuple, node.answer_sets())
        expected = ()
        self.assertTupleEqual(expected, actual,
                              msg="\nExpected: ({})\n  Actual: ({})\n".format(",".join(map(str, expected)),
                                                                              ",".join(map(str, actual))))
