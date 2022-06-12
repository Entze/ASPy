import unittest
from typing import Sequence, Iterable

from aspy.Directive import Directive
from aspy.Literal import BasicLiteral
from aspy.NormalRule import NormalRule
from aspy.Program import Program

a: BasicLiteral = BasicLiteral.make_literal('a')
b: BasicLiteral = BasicLiteral.make_literal('b')
c: BasicLiteral = BasicLiteral.make_literal('c')
d: BasicLiteral = BasicLiteral.make_literal('d')
p: BasicLiteral = BasicLiteral.make_literal('p')
r: BasicLiteral = BasicLiteral.make_literal('r')

nmr_chk = BasicLiteral.make_literal('__nmr_chk')


def body(num: int):
    return BasicLiteral.make_literal('__body', num)


def chk(num: int):
    return BasicLiteral.make_literal('__chk', num)


# noinspection DuplicatedCode
class TestProgramMethods(unittest.TestCase):

    def test_prop_dual_simple_1(self):
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

    def test_prop_dual_unit_1(self):
        r1 = NormalRule(head=p, body=(a,))

        rules = (
            NormalRule(head=-p, body=(-a,)),
            NormalRule(head=-a),
        )
        expected = sorted(rules)

        actual = sorted(Program.propositional_dual((r1,)))
        self.assertListEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}".format("; ".join(map(str, expected)),
                                                                                         "; ".join(map(str, actual))))

    def test_prop_dual_unit_2(self):
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

    def test_prop_call_graph_1(self):
        r1 = NormalRule(head=a, body=(b,))
        r2 = NormalRule(head=b)

        program = Program(rules=(r1, r2))

        actual = program.call_graph

        expected = {
            r1: {b},
            r2: set(),
        }

        self.assertDictEqual(expected, actual)

    def test_prop_call_graph_2(self):
        r1 = NormalRule(head=a, body=(b,))
        r2 = NormalRule(head=b, body=(a,))

        program = Program(rules=(r1, r2))

        actual = program.call_graph

        expected = {
            r1: {a, b},
            r2: {b, a},
        }

        self.assertDictEqual(expected, actual)

    def test_prop_call_graph_3(self):
        r1 = NormalRule(head=a, body=(b,))
        r2 = NormalRule(head=b, body=(a,))
        r3 = NormalRule(head=a, body=(-b,))
        r4 = NormalRule(head=b, body=(-a,))

        program = Program(rules=(r1, r2, r3, r4))

        actual = program.call_graph

        expected = {
            r1: {a, b, -a},
            r2: {a, b, -b},
            r3: {a, -a, -b},
            r4: {b, -a, -b},
        }

        self.assertDictEqual(expected, actual)

    def test_prop_call_graph_4(self):
        r1 = NormalRule(head=a, body=(-b,))
        r2 = NormalRule(head=b, body=(-c,))
        r3 = NormalRule(head=c, body=(-a,))

        program = Program(rules=(r1, r2, r3))

        actual = program.call_graph

        expected = {
            r1: {-b, c, -a},
            r2: {-c, a, -b},
            r3: {-a, b, -c},
        }

        self.assertDictEqual(expected, actual)

    @unittest.skip
    def test_prop_call_graph_5(self):
        r1 = NormalRule(head=a, body=(-b, -c))
        r2 = NormalRule(head=b, body=(-c,))
        r3 = NormalRule(head=c, body=(-a, -d))
        r4 = NormalRule(head=d, body=(-a,))

        program = Program(rules=(r1, r2, r3, r4))

        actual = program.call_graph

        expected = {
            r1: {a, c, d, -a, -b, -c, -d},
            r2: {a, d, -a, -b},
            r3: {a, b, c, d, -a, -b, -c, -d},
            r4: {a, b, c, d, -a, -b, -c, -d},
        }

        self.assertDictEqual(expected, actual)

    def test_prop_sASP_program_dict(self):
        r1 = NormalRule(head=a, body=(-b,))
        d1 = NormalRule(head=-a, body=(b,))
        r2 = NormalRule(head=b, body=(-c,))
        d2 = NormalRule(head=-b, body=(c,))
        r3 = NormalRule(head=c, body=(-a,))
        d3 = NormalRule(head=-c, body=(a,))

        program = Program(rules=(r1, r2, r3))

        actual = dict(program.sASP_program_dict)

        s1 = NormalRule(nmr_chk, (-chk(1), -chk(2), -chk(3)))
        s2 = NormalRule(-chk(1), (a,))
        s3 = NormalRule(-chk(1), (b,))
        s4 = NormalRule(-chk(2), (b,))
        s5 = NormalRule(-chk(2), (c,))
        s6 = NormalRule(-chk(3), (a,))
        s7 = NormalRule(-chk(3), (c,))

        expected = {
            a.signature: dict(primal=[r1], dual=[d1]),
            b.signature: dict(primal=[r2], dual=[d2]),
            c.signature: dict(primal=[r3], dual=[d3]),
            Directive.false().signature: dict(primal=[], dual=[]),
            chk(1).signature: dict(primal=[], dual=[s2, s3, s4, s5, s6, s7]),
            nmr_chk.signature: dict(primal=[s1], dual=[]),
        }

        self.assertDictEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}\n".format(expected, actual))
