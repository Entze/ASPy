import unittest
from typing import Sequence, Iterable

from aspy.Literal import BasicLiteral
from aspy.NormalRule import NormalRule
from aspy.Program import Program

a: BasicLiteral = BasicLiteral.make_literal('a')
b: BasicLiteral = BasicLiteral.make_literal('b')
p: BasicLiteral = BasicLiteral.make_literal('p')
r: BasicLiteral = BasicLiteral.make_literal('r')


def body(num: int):
    return BasicLiteral.make_literal('__body', num)


class TestPropositionalDualMethods(unittest.TestCase):

    def test_simple_1(self):
        r1 = NormalRule(head=p, body=(a, -b))
        r2 = NormalRule(head=p, body=(r,))

        rules = (
            NormalRule(head=-p, body=tuple(sorted((
                -body(1),
                -r,
            )))),
            NormalRule(head=-body(1), body=(-a,)),
            NormalRule(head=-body(1), body=(b,)),
            NormalRule(head=-r),
            NormalRule(head=-a),
            NormalRule(head=-b),
        )
        expected = sorted(rules)

        actual = sorted(Program.propositional_dual((r1, r2)))

        self.assertListEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}".format("; ".join(map(str, expected)),
                                                                                         "; ".join(map(str, actual))))

    def test_unit_1(self):
        r1 = NormalRule(head=p, body=(a,))

        rules = (
            NormalRule(head=-p, body=(-a,)),
            NormalRule(head=-a),
        )
        expected = sorted(rules)

        actual = sorted(Program.propositional_dual((r1,)))
        self.assertListEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}".format("; ".join(map(str, expected)),
                                                                                         "; ".join(map(str, actual))))

    def test_unit_2(self):
        r1 = NormalRule(head=p, body=(a,))
        r2 = NormalRule(head=p, body=(-b,))

        rules = (
            NormalRule(head=-p, body=(-a, b)),
            NormalRule(head=-a),
            NormalRule(head=-b),
        )
        expected = sorted(rules)

        actual = sorted(Program.propositional_dual((r1, r2)))
        self.assertListEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}".format("; ".join(map(str, expected)),
                                                                                         "; ".join(map(str, actual))))
