# noinspection DuplicatedCode
import unittest

from aspy.Comparison import Comparison, ComparisonOperator
from aspy.Literal import BasicLiteral
from aspy.Parse import from_string
from aspy.Program import Program
from aspy.Rule import NormalRule, IntegrityConstraint
from aspy.Symbol import Variable, Function


class TestFromString(unittest.TestCase):

    def test_normal_rule_without_negation(self):
        prg_str = "a :- b."
        a = BasicLiteral.make_literal('a')
        b = BasicLiteral.make_literal('b')
        expected = Program((NormalRule(a, (b,)),))
        actual = from_string(prg_str)
        self.assertEqual(expected, actual, "\nExpected: {}\n  Actual: {}\n".format(expected.fmt(), actual.fmt()))

    def test_with_negation(self):
        prg_str = "a :- b, not c."
        a = BasicLiteral.make_literal('a')
        b = BasicLiteral.make_literal('b')
        c = BasicLiteral.make_literal('c')
        expected = Program((NormalRule(a, (b, -c)),))
        actual = from_string(prg_str)
        self.assertEqual(expected, actual, "\nExpected: {}\n  Actual: {}\n".format(expected.fmt(), actual.fmt()))

    def test_full_program(self):
        prg_str = "a :- not b. b :- not a. :- a, b."
        a = BasicLiteral.make_literal('a')
        b = BasicLiteral.make_literal('b')
        expected = Program((
            NormalRule(a, (-b,)),
            NormalRule(b, (-a,)),
            IntegrityConstraint((a, b)),
        ))
        actual = from_string(prg_str)
        self.assertEqual(expected, actual, "\nExpected: {}\n  Actual: {}\n".format(expected.fmt(), actual.fmt()))

    def test_full_program_variables(self):
        prg_str = "p(X) :- q(X), q(Y), X != Y. nested(this(X), X, Y) :-."
        X = Variable('X')
        Y = Variable('Y')
        p_X = BasicLiteral.make_literal('p', X)
        q_X = BasicLiteral.make_literal('q', X)
        q_Y = BasicLiteral.make_literal('q', Y)
        nested = BasicLiteral.make_literal('nested', Function('this', (X,)), X, Y)
        expected = Program((
            NormalRule(p_X, (q_X, q_Y, Comparison(X, ComparisonOperator.NotEqual, Y))),
            NormalRule(nested),
        ))
        actual = from_string(prg_str)
        self.assertEqual(expected, actual, "\nExpected: {}\n  Actual: {}\n".format(expected.fmt(), actual.fmt()))
