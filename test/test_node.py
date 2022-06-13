import unittest
from typing import cast, Sequence

from aspy.CoinductiveHypothesisSet import CoinductiveHypothesisSet
from aspy.Comparison import Comparison, ComparisonOperator
from aspy.Directive import Directive
from aspy.Goal import Goal
from aspy.Literal import BasicLiteral
from aspy.Node import RuleNode, LiteralNode, Leaf, GoalNode, UnificationNode, ForallNode, DisunificationNode
from aspy.NormalRule import NormalRule
from aspy.Program import RuleMap
from aspy.Symbol import Variable, Function, Term, IntegerConstant

X = Variable('X')
Y = Variable('Y')
F0 = Variable('F0')
F1 = Variable('F1')
F2 = Variable('F2')
F3 = Variable('F3')
A0 = Variable('A0')

p = BasicLiteral.make_literal('p')
p_1 = BasicLiteral.make_literal('p', 1)
p_X = BasicLiteral.make_literal('p', X)
p_Y = BasicLiteral.make_literal('p', Y)

q = BasicLiteral.make_literal('q')
q_0 = BasicLiteral.make_literal('q', 0)
q_1 = BasicLiteral.make_literal('q', 1)
q_2 = BasicLiteral.make_literal('q', 2)
q_X = BasicLiteral.make_literal('q', X)
q_F0 = BasicLiteral.make_literal('q', F0)
q_F1 = BasicLiteral.make_literal('q', F1)
q_F2 = BasicLiteral.make_literal('q', F2)
q_A0 = BasicLiteral.make_literal('q', A0)

r = BasicLiteral.make_literal('r')
s = BasicLiteral.make_literal('s')
t = BasicLiteral.make_literal('t')


def body_fails(num: int, all_quantified: Sequence[Variable] = (), existential_quantified: Sequence[Variable] = ()):
    return BasicLiteral.make_literal('__body_fails', num, Function(arguments=all_quantified),
                                     Function(arguments=existential_quantified))


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

    def test_pred_node_expansion1(self):
        r1 = NormalRule(p_X, (q_X,))
        r2 = NormalRule(q_1)
        d1 = NormalRule(-p_X, (body_fails(1, (X,)),))
        d2 = NormalRule(body_fails(1, (X,)), (-q_X,))
        rule_map: RuleMap = {
            "p/1.": {"primal": [r1], "dual": [d1]},
            "q/1.": {"primal": [r2], "dual": []},
            "__body_fails/3.": {"primal": [d2], "dual": []}
        }

        goal = Goal((p_1,))
        actual = GoalNode(subject=goal)
        actual.init()
        actual.expand(rule_map)

        expected = GoalNode(subject=goal,
                            hypotheses=[CoinductiveHypothesisSet({p_1, q_X}, {X: {Term.one()}}, {X: set()})])

        child_lit_p_1 = LiteralNode(subject=p_1,
                                    parent=expected,
                                    hypotheses=[CoinductiveHypothesisSet({p_1, q_X}, {X: {Term.one()}})])
        expected.children = [child_lit_p_1]
        child_rule_r1 = RuleNode(subject=r1,
                                 parent=child_lit_p_1,
                                 hypotheses=[CoinductiveHypothesisSet({p_1, q_X}, {X: {Term.one()}})])
        child_lit_p_1.children = [child_rule_r1]
        child_lit_q_X = LiteralNode(subject=q_X,
                                    parent=child_rule_r1,
                                    hypotheses=[CoinductiveHypothesisSet({p_1, q_X}, {X: {Term.one()}})])
        child_lit_p_1.children = [child_lit_q_X]
        child_rule_r2 = RuleNode(subject=r2,
                                 parent=child_lit_q_X,
                                 hypotheses=[CoinductiveHypothesisSet({p_1, q_X}, {X: {Term.one()}})])
        child_lit_q_X.children = [child_rule_r2]
        child_success = Leaf.success(child_rule_r2)
        child_rule_r2.children = [child_success]

        self.assertEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}\n".format(expected, actual))

    def test_pred_node_expansion2(self):
        r1 = NormalRule(p_X, (q_X,))
        r2 = NormalRule(q_1)
        d1 = NormalRule(-p_X, (body_fails(1, (X,)),))
        d2 = NormalRule(body_fails(1, (X,)), (-q_X,))
        rule_map: RuleMap = {
            "p/1.": {"primal": [r1], "dual": [d1]},
            "q/1.": {"primal": [r2], "dual": []},
            "__body_fails/3.": {"primal": [d2], "dual": []}
        }

        goal = Goal((p_X,))
        actual = GoalNode(subject=goal)
        actual.init()
        actual.expand(rule_map)

        expected = GoalNode(subject=goal,
                            hypotheses=[CoinductiveHypothesisSet({p_X, q_X}, {X: {Term.one()}}, {X: set()})])

        child_lit_p_X = LiteralNode(subject=p_X,
                                    parent=expected,
                                    hypotheses=[CoinductiveHypothesisSet({p_X, q_X}, {X: {Term.one()}})])
        expected.children = [child_lit_p_X]
        child_rule_r1 = RuleNode(subject=r1,
                                 parent=child_lit_p_X,
                                 hypotheses=[CoinductiveHypothesisSet({p_X, q_X}, {X: {Term.one()}})])
        child_lit_p_X.children = [child_rule_r1]
        child_lit_q_X = LiteralNode(subject=q_X,
                                    parent=child_rule_r1,
                                    hypotheses=[CoinductiveHypothesisSet({p_X, q_X}, {X: {Term.one()}})])
        child_rule_r1.children = [child_lit_q_X]
        child_rule_r2 = RuleNode(subject=r2,
                                 parent=child_lit_q_X,
                                 hypotheses=[CoinductiveHypothesisSet({p_X, q_X}, {X: {Term.one()}})])
        child_lit_q_X.children = [child_rule_r2]
        child_success = Leaf.success(child_rule_r2)
        child_rule_r2.children = [child_success]

        self.assertEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}\n".format(expected, actual))

    def test_pred_node_expansion3(self):
        r1 = NormalRule(p_X, (q_X,))
        r2 = NormalRule(q_1)
        r3 = NormalRule(q_2)
        d1 = NormalRule(-p_X, (body_fails(1, (X,)),))
        d2 = NormalRule(body_fails(1, (X,)), (-q_X,))
        rule_map: RuleMap = {
            "p/1.": {"primal": [r1], "dual": [d1]},
            "q/1.": {"primal": [r2, r3], "dual": []},
            "__body_fails/3.": {"primal": [d2], "dual": []}
        }

        goal = Goal((p_X,))
        actual = GoalNode(subject=goal)
        actual.init()
        actual.expand(rule_map)

        expected = GoalNode(subject=goal,
                            hypotheses=[CoinductiveHypothesisSet({p_X, q_X}, {X: {Term.one()}}, {X: set()}),
                                        CoinductiveHypothesisSet({p_X, q_X}, {X: {Term(IntegerConstant(2))}},
                                                                 {X: set()})
                                        ])

        child_lit_p_X = LiteralNode(subject=p_X,
                                    parent=expected,
                                    hypotheses=[CoinductiveHypothesisSet({p_X, q_X}, {X: {Term.one()}})])
        expected.children = [child_lit_p_X]
        child_rule_r1 = RuleNode(subject=r1,
                                 parent=child_lit_p_X,
                                 hypotheses=[CoinductiveHypothesisSet({p_X, q_X}, {X: {Term.one()}})])
        child_lit_p_X.children = [child_rule_r1]
        child_lit_q_X = LiteralNode(subject=q_X,
                                    parent=child_rule_r1,
                                    hypotheses=[CoinductiveHypothesisSet({p_X, q_X}, {X: {Term.one()}})])
        child_rule_r1.children = [child_lit_q_X]
        child_rule_r2 = RuleNode(subject=r2,
                                 parent=child_lit_q_X,
                                 hypotheses=[CoinductiveHypothesisSet({p_X, q_X}, {X: {Term.one()}})])
        child_rule_r3 = RuleNode(subject=r3,
                                 parent=child_lit_q_X,
                                 hypotheses=[CoinductiveHypothesisSet({p_X, q_X}, {X: {Term.one()}})])
        child_lit_q_X.children = [child_rule_r2, child_rule_r3]
        child_success1 = Leaf.success(child_rule_r2)
        child_rule_r2.children = [child_success1]
        child_success2 = Leaf.success(child_rule_r3)
        child_rule_r3.children = [child_success2]

        self.assertEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}\n".format(expected, actual))

    def test_pred_answer_sets1(self):
        r1 = NormalRule(p_X, (-q,))
        d1 = NormalRule(-p_X, (body_fails(1, (X,)),))
        d2 = NormalRule(body_fails(1, (X,)), (q,))
        d3 = NormalRule(-q)
        rule_map: RuleMap = {
            "p/1.": {"primal": [r1], "dual": [d1]},
            "q/0.": {"primal": [], "dual": [d3]},
            "__body_fails/3.": {"primal": [d2], "dual": []}
        }

        goal = Goal((p_X,))
        query = GoalNode(subject=goal)
        query.init()
        query.expand(rule_map)
        actual = query.answer_sets()
        expected = (CoinductiveHypothesisSet({p_X, -q}),)
        self.assertEqual(expected, actual)

    def test_pred_answer_sets2(self):
        r1 = NormalRule(p_X, (-q,))
        r2 = NormalRule(q, )
        d1 = NormalRule(-p_X, (body_fails(1, (X,)),))
        d2 = NormalRule(body_fails(1, (X,)), (q,))
        rule_map: RuleMap = {
            "p/1.": {"primal": [r1], "dual": [d1]},
            "q/0.": {"primal": [r2], "dual": []},
            "__body_fails/3.": {"primal": [d2], "dual": []}
        }

        goal = Goal((-p_X,))
        query = GoalNode(subject=goal)
        query.init()
        query.expand(rule_map)
        actual = query.answer_sets()
        expected = (CoinductiveHypothesisSet({-p_X, q, body_fails(1, (X,))}),)
        self.assertEqual(expected, actual)

    def test_pred_answer_sets_existential1(self):
        r1 = NormalRule(p_Y, (q_X,))
        r2 = NormalRule(q_0)
        r3 = NormalRule(q_1)

        rule_map: RuleMap = {
            "p/1.": {"primal": [r1]},
            "q/1.": {"primal": [r2, r3]}
        }

        goal = Goal((p_X,))
        query = GoalNode(subject=goal)
        query.init()
        query.expand(rule_map)
        actual = query.answer_sets()
        expected = (CoinductiveHypothesisSet({p_X, q_F2},
                                             {F2: {Term.zero()}},
                                             {F2: set()}), CoinductiveHypothesisSet({p_X, q_F2},
                                                                                    {F2: {Term.one()}},
                                                                                    {F2: set()}))
        self.assertEqual(expected, actual)

    def test_pred_answer_sets_existential2(self):
        r1 = NormalRule(p_Y, (q_F0,))
        r2 = NormalRule(q_0)
        r3 = NormalRule(q_1)

        rule_map: RuleMap = {
            "p/1.": {"primal": [r1]},
            "q/1.": {"primal": [r2, r3]}
        }

        goal = Goal((p_X,))
        query = GoalNode(subject=goal)
        query.init()
        query.expand(rule_map)
        actual = query.answer_sets()
        expected = (CoinductiveHypothesisSet({p_X, q_F0},
                                             {F0: {Term.zero()}},
                                             {F0: set()}),
                    CoinductiveHypothesisSet({p_X, q_F0}, {F0: {Term.one()}}, {F0: set()}))
        self.assertEqual(expected, actual)

    def test_pred_answer_sets_unification(self):
        r1 = NormalRule(q, (q_X, p_Y, Comparison(X, ComparisonOperator.Equal, Y)))
        r2 = NormalRule(q_0)
        r3 = NormalRule(q_1)
        r4 = NormalRule(q_2)
        r5 = NormalRule(p_1)
        rule_map: RuleMap = {
            "q/0.": {"primal": [r1]},
            "q/1.": {"primal": [r2, r3, r4]},
            "p/1.": {"primal": [r5]},
        }

        goal = Goal((q,))
        query = GoalNode(subject=goal)
        query.init()
        query.expand(rule_map)
        actual = query.answer_sets()
        expected = (
            CoinductiveHypothesisSet({q, q_X, p_Y}, {X: {Term.one(), Y}, Y: {Term.one(), X}}, {X: set(), Y: set()}),)
        self.assertEqual(actual, expected, msg="\nExpected: {}\n  Actual: {}\n".format(expected, actual))


class TestUnificationNode(unittest.TestCase):

    def test_simple(self):
        A = Variable('A')
        B = Variable('B')
        comp = Comparison(A, ComparisonOperator.Equal, B)

        chs = [CoinductiveHypothesisSet(prohibited={A: {Term.zero()}, B: set()}),
               CoinductiveHypothesisSet(prohibited={A: {Term.one()}, B: set()})]
        node = UnificationNode(subject=comp, hypotheses=chs)
        node.expand()
        actual = node.hypotheses
        expected = [
            CoinductiveHypothesisSet(bindings={A: {B}, B: {A}}, prohibited={A: {Term.zero()}, B: {Term.zero()}}),
            CoinductiveHypothesisSet(bindings={A: {B}, B: {A}}, prohibited={A: {Term.one()}, B: {Term.one()}})]
        self.assertEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}\n".format(expected, actual))


# noinspection DuplicatedCode
class TestForallNode(unittest.TestCase):

    def test_simple1(self):
        r1 = NormalRule(r, (Directive.forall((X,), q_X.atom.symbol)))
        r2 = NormalRule(q_X)

        rule_map: RuleMap = {
            "r/0.": {"primal": [r1]},
            "q/1.": {"primal": [r2]}
        }

        actual = ForallNode(subject=Directive.forall((X,), q_X.atom.symbol))
        actual.init()
        actual.expand(rule_map)

        expected = ForallNode(subject=Directive.forall((X,), q_X.atom.symbol),
                              hypotheses=[CoinductiveHypothesisSet({q_A0})])
        query_q_X_lit = GoalNode(subject=Goal((q_A0,)),
                                 parent=expected,
                                 hypotheses=[CoinductiveHypothesisSet({q_A0})])
        expected.children = [query_q_X_lit]
        child_q_X_lit = LiteralNode(subject=q_A0,
                                    parent=query_q_X_lit,
                                    hypotheses=[CoinductiveHypothesisSet({q_A0})])
        query_q_X_lit.children = [child_q_X_lit]
        child_r2_rule = RuleNode(subject=r2,
                                 parent=child_q_X_lit,
                                 hypotheses=[CoinductiveHypothesisSet({q_A0})])
        child_q_X_lit.children = [child_r2_rule]

        child_success = Leaf.success(child_r2_rule)
        child_r2_rule.children = [child_success]

        self.assertEqual(expected, actual)

    def test_simple2(self):
        r1 = NormalRule(r, (Directive.forall((X,), q_X.atom.symbol)))
        r2 = NormalRule(q_X, (Comparison(X, ComparisonOperator.NotEqual, Term.one()),))
        r3 = NormalRule(q_1)

        rule_map: RuleMap = {
            "r/0.": {"primal": [r1]},
            "q/1.": {"primal": [r2, r3]}
        }

        actual = ForallNode(subject=Directive.forall((X,), q_X.atom.symbol))
        actual.init()
        actual.expand(rule_map)

        expected = ForallNode(subject=Directive.forall((X,), q_X.atom.symbol),
                              hypotheses=[CoinductiveHypothesisSet({q_A0})])

        query_q_X_lit = GoalNode(subject=Goal((q_A0,)),
                                 parent=expected,
                                 hypotheses=[CoinductiveHypothesisSet({q_A0}, prohibited={A0: {Term.one()}}),
                                             CoinductiveHypothesisSet({q_A0}, bindings={A0: {Term.one()}}),
                                             ])

        child_q_X_lit = LiteralNode(subject=q_A0,
                                    parent=query_q_X_lit,
                                    hypotheses=[CoinductiveHypothesisSet({q_A0}, prohibited={A0: {Term.one()}}),
                                                CoinductiveHypothesisSet({q_A0}, bindings={A0: {Term.one()}}),
                                                ])
        query_q_X_lit.children = [child_q_X_lit]

        child_r2_rule = RuleNode(subject=r2,
                                 parent=child_q_X_lit,
                                 hypotheses=[CoinductiveHypothesisSet({q_A0}, prohibited={A0: {Term.one()}}),
                                             CoinductiveHypothesisSet({q_A0}, bindings={A0: {Term.one()}}),
                                             ])
        child_r3_rule = RuleNode(subject=r3,
                                 parent=child_q_X_lit,
                                 hypotheses=[CoinductiveHypothesisSet({q_A0}, prohibited={A0: {Term.one()}}),
                                             CoinductiveHypothesisSet({q_A0}, bindings={A0: {Term.one()}}),
                                             ]
                                 )
        child_q_X_lit.children = [child_r2_rule, child_r3_rule]

        child_disunification = DisunificationNode(subject=Comparison(X, ComparisonOperator.NotEqual, Term.one()),
                                                  parent=child_r2_rule,
                                                  hypotheses=[
                                                      CoinductiveHypothesisSet({q_A0}, prohibited={A0: {Term.one()}}),
                                                      CoinductiveHypothesisSet({q_A0}, bindings={A0: {Term.one()}}),
                                                  ]
                                                  )

        child_r2_rule.children = [child_disunification]
        child_success1 = Leaf.success(child_disunification)
        child_disunification.children = [child_success1]

        child_success2 = Leaf.success(child_r3_rule)
        child_r3_rule.children = [child_success2]

        query_q_constrained = GoalNode(subject=Goal((q_A0,)),
                                       parent=expected,
                                       hypotheses=[
                                           CoinductiveHypothesisSet({q_A0}, bindings={A0: {Term.one()}})
                                       ])

        child_q_X_constrained = LiteralNode(subject=q_A0,
                                            parent=query_q_constrained,
                                            hypotheses=[
                                                CoinductiveHypothesisSet({q_A0}, bindings={A0: {Term.one()}})
                                            ])
        query_q_constrained.children = [child_q_X_constrained]
        expected.children = [query_q_X_lit, query_q_constrained]
        child_r2_constrained = RuleNode(subject=r2,
                                        parent=child_q_X_constrained,
                                        hypotheses=[])

        query_q_constrained.children = [child_r2_constrained]

        child_disunification_constrained = DisunificationNode(
            subject=Comparison(X, ComparisonOperator.NotEqual, Term.one()),
            parent=child_r2_constrained,
            hypotheses=[])
        child_r2_constrained.children = [child_disunification_constrained]
        disunification_fail = Leaf.fail(child_disunification_constrained)
        child_disunification_constrained.children = [disunification_fail]

        child_r3_constrained = RuleNode(subject=r3,
                                        parent=child_q_X_constrained,
                                        hypotheses=[CoinductiveHypothesisSet({q_A0}, prohibited={A0: {Term.one()}}),
                                                    CoinductiveHypothesisSet({q_A0}, bindings={A0: {Term.one()}}),
                                                    ]
                                        )
        child_q_X_constrained.children = [child_r2_constrained, child_r3_constrained]

        child_success3 = Leaf.success(child_r3_constrained)
        child_r3_constrained.children = [child_success3]

        self.assertEqual(expected, actual)

    def test_simple3(self):
        r1 = NormalRule(r, (Directive.forall((X,), q_X.atom.symbol)))
        r2 = NormalRule(q_1)

        rule_map: RuleMap = {
            "r/0.": {"primal": [r1]},
            "q/1.": {"primal": [r2]}
        }

        actual = ForallNode(subject=Directive.forall((X,), q_X.atom.symbol))
        actual.init()
        actual.expand(rule_map)

        expected = ForallNode(subject=Directive.forall((X,), q_X.atom.symbol),
                              hypotheses=[CoinductiveHypothesisSet()])
        query_q_X_lit = GoalNode(subject=Goal((q_A0,)),
                                 parent=expected,
                                 hypotheses=[CoinductiveHypothesisSet({q_A0}, bindings={A0: {Term.one()}})])
        child_q_X_lit = LiteralNode(subject=q_A0,
                                    parent=query_q_X_lit,
                                    hypotheses=[CoinductiveHypothesisSet({q_A0}, bindings={A0: {Term.one()}})])
        query_q_X_lit.children = [child_q_X_lit]
        child_r2_rule = RuleNode(subject=r2,
                                 parent=child_q_X_lit,
                                 hypotheses=[CoinductiveHypothesisSet({q_A0}, bindings={A0: {Term.one()}})])
        child_q_X_lit.children = [child_r2_rule]

        child_success = Leaf.success(child_r2_rule)
        child_r2_rule.children = [child_success]
        child_fail = Leaf.fail(expected)
        expected.children = [query_q_X_lit, child_fail]

        self.assertEqual(expected, actual)
