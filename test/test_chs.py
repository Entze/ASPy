# noinspection DuplicatedCode
import unittest

from aspy.CoinductiveHypothesisSet import CoinductiveHypothesisSet
from aspy.Symbol import Variable, Term, Function


class TestNodeMethods(unittest.TestCase):

    def test_unifies_negvar_nonvar_empty_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: set()})
        actual = chs.unifies(negvar, nonvar)
        expected = True
        self.assertEquals(expected, actual)

    def test_unifies_negvar_term_negative(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {nonvar}})
        actual = chs.unifies(negvar, nonvar)
        expected = True
        self.assertEquals(expected, actual)

    def test_unifies_term_negvar_negative(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {nonvar}})
        actual = chs.unifies(nonvar, negvar)
        expected = True
        self.assertEquals(expected, actual)

    def test_unifies_negvar_function_negative(self):
        negvar = Variable('A')
        nonvar = Function('a')

        chs = CoinductiveHypothesisSet(prohibited={negvar: {nonvar}})
        actual = chs.unifies(negvar, nonvar)
        expected = True
        self.assertEquals(expected, actual)

    def test_unifies_negvar_function_compound_negative(self):
        negvar = Variable('A')
        var = Variable('B')
        nonvar = Function('a', (var,))

        chs = CoinductiveHypothesisSet(prohibited={negvar: {nonvar}, var: {Term.one()}})
        actual = chs.unifies(negvar, nonvar)
        expected = True
        self.assertEquals(expected, actual)

    def test_unifies_negvar_nonvar_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {Term.zero()}})
        actual = chs.unifies(negvar, nonvar)
        expected = True
        self.assertEquals(expected, actual)

    def test_unifies_nonvar_negvar_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {Term.zero()}})
        actual = chs.unifies(nonvar, negvar)
        expected = True
        self.assertEquals(expected, actual)

    def test_constructive_unification_negvar_nonvar_empty_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: set()})
        actual = chs.constructive_unification(negvar, nonvar)
        expected = CoinductiveHypothesisSet(bindings={negvar: {nonvar}}, prohibited={negvar: set()})
        self.assertEquals(expected, actual, msg="\nExpected: {}\n  Actual: {}\n".format(expected, actual))

    def test_constructive_unification_negvar_nonvar_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {Term.zero()}})
        actual = chs.constructive_unification(negvar, nonvar)
        expected = CoinductiveHypothesisSet(bindings={negvar: {nonvar}}, prohibited={negvar: {Term.zero()}})
        self.assertEquals(expected, actual)

    def test_constructive_unification_nonvar_negvar_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {Term.zero()}})
        actual = chs.constructive_unification(nonvar, negvar)
        expected = CoinductiveHypothesisSet(bindings={negvar: {nonvar}}, prohibited={negvar: {Term.zero()}})
        self.assertEquals(expected, actual)
