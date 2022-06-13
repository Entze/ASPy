# noinspection DuplicatedCode
import unittest
from collections import defaultdict

from aspy.CoinductiveHypothesisSet import CoinductiveHypothesisSet
from aspy.Literal import BasicLiteral
from aspy.NormalRule import NormalRule
from aspy.Symbol import Variable, Term, Function

X = Variable('X')
Y = Variable('Y')
F0 = Variable('F0')
F1 = Variable('F1')

a = BasicLiteral.make_literal('a')
a_X = BasicLiteral.make_literal('a', X)
a_Y = BasicLiteral.make_literal('a', Y)
a_F0 = BasicLiteral.make_literal('a', F0)
a_F1 = BasicLiteral.make_literal('a', F1)

q = BasicLiteral.make_literal('q')
q_X = BasicLiteral.make_literal('q', X)
q_Y = BasicLiteral.make_literal('q', Y)
q_F0 = BasicLiteral.make_literal('q', F0)

p = BasicLiteral.make_literal('p')
p_1 = BasicLiteral.make_literal('p', 1)

p_X = BasicLiteral.make_literal('p', X)
p_Y = BasicLiteral.make_literal('p', Y)
p_F0 = BasicLiteral.make_literal('p', F0)


# noinspection DuplicatedCode
class TestUnifies(unittest.TestCase):

    def test_negvar_nonvar_empty_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: set()})
        actual = chs.unifies(negvar, nonvar)
        expected = True
        self.assertEqual(expected, actual)

    def test_negvar_term_negative(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {nonvar}})
        actual = chs.unifies(negvar, nonvar)
        expected = False
        self.assertEqual(expected, actual)

    def test_term_negvar_negative(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {nonvar}})
        actual = chs.unifies(nonvar, negvar)
        expected = False
        self.assertEqual(expected, actual)

    def test_negvar_function_negative(self):
        negvar = Variable('A')
        nonvar = Function('a')

        chs = CoinductiveHypothesisSet(prohibited={negvar: {nonvar}})
        actual = chs.unifies(negvar, nonvar)
        expected = False
        self.assertEqual(expected, actual)

    def test_negvar_function_compound_negative(self):
        negvar = Variable('A')
        var = Variable('B')
        nonvar = Function('a', (var,))

        chs = CoinductiveHypothesisSet(prohibited={negvar: {nonvar}, var: {Term.one()}})
        actual = chs.unifies(negvar, nonvar)
        expected = False
        self.assertEqual(expected, actual)

    def test_negvar_nonvar_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {Term.zero()}})
        actual = chs.unifies(negvar, nonvar)
        expected = True
        self.assertEqual(expected, actual)

    def test_nonvar_negvar_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {Term.zero()}})
        actual = chs.unifies(nonvar, negvar)
        expected = True
        self.assertEqual(expected, actual)

    def test_var_var_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet()
        actual = chs.unifies(var1, var2)
        expected = True
        self.assertEqual(expected, actual)

    def test_var_var_bound_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(bindings={var1: {Term.zero()}, var2: {Term.one()}})
        actual = chs.unifies(var1, var2)
        expected = True
        self.assertEqual(expected, actual)

    def test_var_var_partiallybound1_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(bindings={var1: {Term.zero()}})
        actual = chs.unifies(var1, var2)
        expected = True
        self.assertEqual(expected, actual)

    def test_var_var_partiallybound2_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(bindings={var2: {Term.zero()}})
        actual = chs.unifies(var1, var2)
        expected = True
        self.assertEqual(expected, actual)

    def test_var_var_prohibited_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(prohibited={var1: {Term.zero()}, var2: {Term.one()}})
        actual = chs.unifies(var1, var2)
        expected = True
        self.assertEqual(expected, actual)

    def test_var_var_prohibitedpartially1_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(prohibited={var1: {Term.zero()}})
        actual = chs.unifies(var1, var2)
        expected = True
        self.assertEqual(expected, actual)

    def test_var_var_prohibitedpartially2_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(prohibited={var2: {Term.zero()}})
        actual = chs.unifies(var1, var2)
        expected = True
        self.assertEqual(expected, actual)

    def test_compound_compound_positive(self):
        fun1 = Function('a', (Term.one(), Function('b', (Function('c'), Term.zero()))))
        fun2 = Function('a', (Term.one(), Function('b', (Function('c'), Term.zero()))))

        chs = CoinductiveHypothesisSet()
        actual = chs.unifies(fun1, fun2)
        expected = True
        self.assertEqual(expected, actual)

    def test_compound_compound_with_var_positive(self):
        var = Variable('A')
        fun1 = Function('a', (Term.one(), Function('b', (Function('c'), Term.zero()))))
        fun2 = Function('a', (Term.one(), Function('b', (var, Term.zero()))))

        chs = CoinductiveHypothesisSet()
        actual = chs.unifies(fun1, fun2)
        expected = True
        self.assertEqual(expected, actual)

    def test_compound_compound_mismatching_args_negative(self):
        fun1 = Function('a', (Term.one(),))
        fun2 = Function('a', (Term.zero(),))

        chs = CoinductiveHypothesisSet()
        actual = chs.unifies(fun1, fun2)
        expected = False
        self.assertEqual(expected, actual)

    def test_compound_compound_mismatching_arity_negative(self):
        fun1 = Function('a', (Term.one(),))
        fun2 = Function('a', (Term.zero(), Term.one()))

        chs = CoinductiveHypothesisSet()
        actual = chs.unifies(fun1, fun2)
        expected = False
        self.assertEqual(expected, actual)

    def test_compound_compound_mismatching_name_negative(self):
        fun1 = Function('a', (Term.one(),))
        fun2 = Function('b', (Term.one(),))

        chs = CoinductiveHypothesisSet()
        actual = chs.unifies(fun1, fun2)
        expected = False
        self.assertEqual(expected, actual)

    def test_compound_compound_mismatching_var_negative(self):
        var = Variable('A')
        fun1 = Function('a', (var,))
        fun2 = Function('a', (Term.one(),))

        chs = CoinductiveHypothesisSet(prohibited={var: {Term.one()}})
        actual = chs.unifies(fun1, fun2)
        expected = False
        self.assertEqual(expected, actual)

    def test_compound_compound_mismatching_var_reversed_negative(self):
        var = Variable('A')
        fun1 = Function('a', (Term.one(),))
        fun2 = Function('a', (var,))

        chs = CoinductiveHypothesisSet(prohibited={var: {Term.one()}})
        actual = chs.unifies(fun1, fun2)
        expected = False
        self.assertEqual(expected, actual)

    def test_term_term_positive(self):
        term1 = Term.zero()
        term2 = Term.zero()

        chs = CoinductiveHypothesisSet()
        actual = chs.unifies(term1, term2)
        expected = True
        self.assertEqual(expected, actual)

    def test_term_term_negative(self):
        term1 = Term.zero()
        term2 = Term.one()

        chs = CoinductiveHypothesisSet()
        actual = chs.unifies(term1, term2)
        expected = False
        self.assertEqual(expected, actual)


# noinspection DuplicatedCode
class TestConstructiveUnification(unittest.TestCase):

    def test_negvar_nonvar_empty_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: set()})
        actual = chs.constructive_unification(negvar, nonvar)
        expected = (CoinductiveHypothesisSet(bindings={negvar: {nonvar}}, prohibited={negvar: set()}),)
        self.assertEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}\n".format(expected, actual))

    def test_negvar_term_negative(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {nonvar}})
        actual = chs.constructive_unification(negvar, nonvar)
        expected = ()
        self.assertEqual(expected, actual)

    def test_term_negvar_negative(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {nonvar}})
        actual = chs.constructive_unification(nonvar, negvar)
        expected = ()
        self.assertEqual(expected, actual)

    def test_negvar_function_negative(self):
        negvar = Variable('A')
        nonvar = Function('a')

        chs = CoinductiveHypothesisSet(prohibited={negvar: {nonvar}})
        actual = chs.constructive_unification(negvar, nonvar)
        expected = ()
        self.assertEqual(expected, actual)

    def test_negvar_function_compound_negative(self):
        negvar = Variable('A')
        var = Variable('B')
        nonvar = Function('a', (var,))

        chs = CoinductiveHypothesisSet(prohibited={negvar: {nonvar}, var: {Term.one()}})
        actual = chs.constructive_unification(negvar, nonvar)
        expected = ()
        self.assertEqual(expected, actual)

    def test_negvar_nonvar_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {Term.zero()}})
        actual = chs.constructive_unification(negvar, nonvar)
        expected = (CoinductiveHypothesisSet(bindings={negvar: {nonvar}}, prohibited={negvar: {Term.zero()}}),)
        self.assertEqual(expected, actual)

    def test_nonvar_negvar_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {Term.zero()}})
        actual = chs.constructive_unification(nonvar, negvar)
        expected = (CoinductiveHypothesisSet(bindings={negvar: {nonvar}}, prohibited={negvar: {Term.zero()}}),)
        self.assertEqual(expected, actual)

    def test_var_var_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet()
        actual = chs.constructive_unification(var1, var2)
        expected = (
            CoinductiveHypothesisSet(bindings={var1: {var2}, var2: {var1}}, prohibited={var1: set(), var2: set()}),)
        self.assertEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}\n".format(expected, actual))

    def test_var_var_bound_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(bindings={var1: {Term.zero()}, var2: {Term.one()}})
        actual = chs.constructive_unification(var1, var2)
        expected = (CoinductiveHypothesisSet(bindings={var1: {Term.zero(), var2}, var2: {Term.one(), var1}},
                                             prohibited={var1: set(), var2: set()}),)
        self.assertEqual(expected, actual)

    def test_var_var_partiallybound1_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(bindings={var1: {Term.zero()}, var2: set()})
        actual = chs.constructive_unification(var1, var2)
        expected = (CoinductiveHypothesisSet(bindings={var1: {Term.zero(), var2}, var2: {var1}},
                                             prohibited={var1: set(), var2: set()}),)
        self.assertEqual(expected, actual)

    def test_var_var_partiallybound2_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(bindings={var2: {Term.zero()}, var1: set()})
        actual = chs.constructive_unification(var1, var2)
        expected = (CoinductiveHypothesisSet(bindings={var1: {var2}, var2: {Term.zero(), var1}},
                                             prohibited={var1: set(), var2: set()}),)
        self.assertEqual(expected, actual)

    def test_var_var_prohibited_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(prohibited={var1: {Term.zero()}, var2: {Term.one()}})
        actual = chs.constructive_unification(var1, var2)
        expected = (CoinductiveHypothesisSet(bindings={var1: {var2}, var2: {var1}},
                                             prohibited={var1: {Term.zero(), Term.one()},
                                                         var2: {Term.zero(), Term.one()}}),)
        self.assertEqual(expected, actual)

    def test_var_var_prohibitedpartially1_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(prohibited={var1: {Term.zero()}, var2: set()})
        actual = chs.constructive_unification(var1, var2)
        expected = (CoinductiveHypothesisSet(bindings={var1: {var2}, var2: {var1}},
                                             prohibited={var1: {Term.zero()}, var2: {Term.zero()}}),)
        self.assertEqual(expected, actual)

    def test_var_var_prohibitedpartially2_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(prohibited={var2: {Term.zero()}, var1: set()})
        actual = chs.constructive_unification(var1, var2)
        expected = (CoinductiveHypothesisSet(bindings={var1: {var2}, var2: {var1}},
                                             prohibited={var1: {Term.zero()}, var2: {Term.zero()}}),)
        self.assertEqual(expected, actual)

    def test_compound_compound_positive(self):
        fun1 = Function('a', (Term.one(), Function('b', (Function('c'), Term.zero()))))
        fun2 = Function('a', (Term.one(), Function('b', (Function('c'), Term.zero()))))

        chs = CoinductiveHypothesisSet()
        actual = chs.constructive_unification(fun1, fun2)
        expected = (CoinductiveHypothesisSet(),)
        self.assertEqual(expected, actual)

    def test_compound_compound_with_var_positive(self):
        var = Variable('A')
        fun1 = Function('a', (Term.one(), Function('b', (Function('c'), Term.zero()))))
        fun2 = Function('a', (Term.one(), Function('b', (var, Term.zero()))))

        chs = CoinductiveHypothesisSet()
        actual = chs.constructive_unification(fun1, fun2)
        expected = (CoinductiveHypothesisSet(bindings={var: {Function('c')}}, prohibited={var: set()}),)
        self.assertEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}\n".format(expected, actual))

    def test_compound_compound_mismatching_args_negative(self):
        fun1 = Function('a', (Term.one(),))
        fun2 = Function('a', (Term.zero(),))

        chs = CoinductiveHypothesisSet()
        actual = chs.constructive_unification(fun1, fun2)
        expected = ()
        self.assertEqual(expected, actual)

    def test_compound_compound_mismatching_arity_negative(self):
        fun1 = Function('a', (Term.one(),))
        fun2 = Function('a', (Term.zero(), Term.one()))

        chs = CoinductiveHypothesisSet()
        actual = chs.constructive_unification(fun1, fun2)
        expected = ()
        self.assertEqual(expected, actual)

    def test_compound_compound_mismatching_name_negative(self):
        fun1 = Function('a', (Term.one(),))
        fun2 = Function('b', (Term.one(),))

        chs = CoinductiveHypothesisSet()
        actual = chs.constructive_unification(fun1, fun2)
        expected = ()
        self.assertEqual(expected, actual)

    def test_compound_compound_mismatching_var_negative(self):
        var = Variable('A')
        fun1 = Function('a', (var,))
        fun2 = Function('a', (Term.one(),))

        chs = CoinductiveHypothesisSet(prohibited={var: {Term.one()}})
        actual = chs.constructive_unification(fun1, fun2)
        expected = ()
        self.assertEqual(expected, actual)

    def test_compound_compound_mismatching_var_reversed_negative(self):
        var = Variable('A')
        fun1 = Function('a', (Term.one(),))
        fun2 = Function('a', (var,))

        chs = CoinductiveHypothesisSet(prohibited={var: {Term.one()}})
        actual = chs.constructive_unification(fun1, fun2)
        expected = ()
        self.assertEqual(expected, actual)

    def test_term_term_positive(self):
        term1 = Term.zero()
        term2 = Term.zero()

        chs = CoinductiveHypothesisSet()
        actual = chs.constructive_unification(term1, term2)
        expected = (CoinductiveHypothesisSet(),)
        self.assertEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}\n".format(expected, actual))

    def test_term_term_negative(self):
        term1 = Term.zero()
        term2 = Term.one()

        chs = CoinductiveHypothesisSet()
        actual = chs.constructive_unification(term1, term2)
        expected = ()
        self.assertEqual(expected, actual)


# noinspection DuplicatedCode
class TestDisunifies(unittest.TestCase):

    def test_negvar_nonvar_empty_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: set()})
        actual = chs.disunifies(negvar, nonvar)
        expected = True
        self.assertEqual(expected, actual)

    def test_negvar_term_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {nonvar}})
        actual = chs.disunifies(negvar, nonvar)
        expected = True
        self.assertEqual(expected, actual)

    def test_term_negvar_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {nonvar}})
        actual = chs.disunifies(nonvar, negvar)
        expected = True
        self.assertEqual(expected, actual)

    def test_negvar_function_positive(self):
        negvar = Variable('A')
        nonvar = Function('a')

        chs = CoinductiveHypothesisSet(prohibited={negvar: {nonvar}})
        actual = chs.disunifies(negvar, nonvar)
        expected = True
        self.assertEqual(expected, actual)

    def test_negvar_function_compound_positive(self):
        negvar = Variable('A')
        var = Variable('B')
        nonvar = Function('a', (var,))

        chs = CoinductiveHypothesisSet(prohibited={negvar: {nonvar}, var: {Term.one()}})
        actual = chs.disunifies(negvar, nonvar)
        expected = True
        self.assertEqual(expected, actual)

    def test_negvar_nonvar_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {Term.zero()}})
        actual = chs.disunifies(negvar, nonvar)
        expected = True
        self.assertEqual(expected, actual)

    def test_nonvar_negvar_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {Term.zero()}})
        actual = chs.disunifies(nonvar, negvar)
        expected = True
        self.assertEqual(expected, actual)

    def test_var_var_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet()
        actual = chs.disunifies(var1, var2)
        expected = True
        self.assertEqual(expected, actual)

    def test_var_var_bound_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(bindings={var1: {Term.zero()}, var2: {Term.one()}})
        actual = chs.disunifies(var1, var2)
        expected = True
        self.assertEqual(expected, actual)

    def test_var_var_partiallybound1_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(bindings={var1: {Term.zero()}, var2: set()})
        actual = chs.disunifies(var1, var2)
        expected = True
        self.assertEqual(expected, actual)

    def test_var_var_partiallybound2_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(bindings={var2: {Term.zero()}, var1: set()})
        actual = chs.disunifies(var1, var2)
        expected = True
        self.assertEqual(expected, actual)

    def test_var_var_prohibited_error(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(prohibited={var1: {Term.zero()}, var2: {Term.one()}})
        with self.assertRaises(Exception):
            chs.disunifies(var1, var2)

    def test_var_var_prohibitedpartially1_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(prohibited={var1: {Term.zero()}})
        actual = chs.disunifies(var1, var2)
        expected = True
        self.assertEqual(expected, actual)

    def test_var_var_prohibitedpartially2_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(prohibited={var2: {Term.zero()}})
        actual = chs.disunifies(var1, var2)
        expected = True
        self.assertEqual(expected, actual)

    def test_compound_compound_negative(self):
        fun1 = Function('a', (Term.one(), Function('b', (Function('c'), Term.zero()))))
        fun2 = Function('a', (Term.one(), Function('b', (Function('c'), Term.zero()))))

        chs = CoinductiveHypothesisSet()
        actual = chs.disunifies(fun1, fun2)
        expected = False
        self.assertEqual(expected, actual)

    def test_compound_compound_with_var_positive(self):
        var = Variable('A')
        fun1 = Function('a', (Term.one(), Function('b', (Function('c'), Term.zero()))))
        fun2 = Function('a', (Term.one(), Function('b', (var, Term.zero()))))

        chs = CoinductiveHypothesisSet()
        actual = chs.disunifies(fun1, fun2)
        expected = True
        self.assertEqual(expected, actual)

    def test_compound_compound_mismatching_args_positive(self):
        fun1 = Function('a', (Term.one(),))
        fun2 = Function('a', (Term.zero(),))

        chs = CoinductiveHypothesisSet()
        actual = chs.disunifies(fun1, fun2)
        expected = True
        self.assertEqual(expected, actual)

    def test_compound_compound_mismatching_arity_positive(self):
        fun1 = Function('a', (Term.one(),))
        fun2 = Function('a', (Term.zero(), Term.one()))

        chs = CoinductiveHypothesisSet()
        actual = chs.disunifies(fun1, fun2)
        expected = True
        self.assertEqual(expected, actual)

    def test_compound_compound_mismatching_name_positive(self):
        fun1 = Function('a', (Term.one(),))
        fun2 = Function('b', (Term.one(),))

        chs = CoinductiveHypothesisSet()
        actual = chs.disunifies(fun1, fun2)
        expected = True
        self.assertEqual(expected, actual)

    def test_compound_compound_mismatching_var_positive(self):
        var = Variable('A')
        fun1 = Function('a', (var,))
        fun2 = Function('a', (Term.one(),))

        chs = CoinductiveHypothesisSet(prohibited={var: {Term.one()}})
        actual = chs.disunifies(fun1, fun2)
        expected = True
        self.assertEqual(expected, actual)

    def test_compound_compound_mismatching_var_reversed_positive(self):
        var = Variable('A')
        fun1 = Function('a', (Term.one(),))
        fun2 = Function('a', (var,))

        chs = CoinductiveHypothesisSet(prohibited={var: {Term.one()}})
        actual = chs.disunifies(fun1, fun2)
        expected = True
        self.assertEqual(expected, actual)

    def test_term_term_negative(self):
        term1 = Term.zero()
        term2 = Term.zero()

        chs = CoinductiveHypothesisSet()
        actual = chs.disunifies(term1, term2)
        expected = False
        self.assertEqual(expected, actual)

    def test_term_term_positive(self):
        term1 = Term.zero()
        term2 = Term.one()

        chs = CoinductiveHypothesisSet()
        actual = chs.disunifies(term1, term2)
        expected = True
        self.assertEqual(expected, actual)


# noinspection DuplicatedCode
class TestConstructiveDisjunification(unittest.TestCase):

    def test_negvar_nonvar_empty_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: set()})
        actual = chs.constructive_disunification(negvar, nonvar)
        expected = (CoinductiveHypothesisSet(prohibited={negvar: {nonvar}}),)
        self.assertEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}\n".format(expected, actual))

    def test_negvar_term_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {nonvar}})
        actual = chs.constructive_disunification(negvar, nonvar)
        expected = (CoinductiveHypothesisSet(prohibited={negvar: {nonvar}}),)
        self.assertEqual(expected, actual)

    def test_term_negvar_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {nonvar}})
        actual = chs.constructive_disunification(nonvar, negvar)
        expected = (CoinductiveHypothesisSet(prohibited={negvar: {nonvar}}),)
        self.assertEqual(expected, actual)

    def test_negvar_function_positive(self):
        negvar = Variable('A')
        nonvar = Function('a')

        chs = CoinductiveHypothesisSet(prohibited={negvar: {nonvar}})
        actual = chs.constructive_disunification(negvar, nonvar)
        expected = (CoinductiveHypothesisSet(prohibited={negvar: {nonvar}}),)
        self.assertEqual(expected, actual)

    def test_negvar_function_compound_positive(self):
        negvar = Variable('A')
        var = Variable('B')
        nonvar = Function('a', (var,))

        chs = CoinductiveHypothesisSet(prohibited={negvar: {nonvar}, var: {Term.one()}})
        actual = chs.constructive_disunification(negvar, nonvar)
        expected = (CoinductiveHypothesisSet(prohibited={negvar: {nonvar}, var: {Term.one()}}),)
        self.assertEqual(expected, actual)

    def test_negvar_nonvar_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {Term.zero()}})
        actual = chs.constructive_disunification(negvar, nonvar)
        expected = (CoinductiveHypothesisSet(prohibited={negvar: {Term.zero(), Term.one()}}),)
        self.assertEqual(expected, actual)

    def test_nonvar_negvar_positive(self):
        negvar = Variable('A')
        nonvar = Term.one()

        chs = CoinductiveHypothesisSet(prohibited={negvar: {Term.zero()}})
        actual = chs.constructive_disunification(nonvar, negvar)
        expected = (CoinductiveHypothesisSet(prohibited={negvar: {Term.zero(), Term.one()}}),)
        self.assertEqual(expected, actual)

    def test_var_var_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet()
        actual = chs.constructive_disunification(var1, var2)
        expected = (
            CoinductiveHypothesisSet(prohibited={var1: {var2}, var2: {var1}}),)
        self.assertEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}\n".format(expected, actual))

    def test_var_var_bound_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(bindings={var1: {Term.zero()}, var2: {Term.one()}})
        actual = chs.constructive_disunification(var1, var2)
        expected = (CoinductiveHypothesisSet(bindings={var1: {Term.zero()}, var2: {Term.one()}},
                                             prohibited={var1: {var2}, var2: {var1}}),)
        self.assertEqual(expected, actual)

    def test_var_var_partiallybound1_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(bindings={var1: {Term.zero()}, var2: set()})
        actual = chs.constructive_disunification(var1, var2)
        expected = (CoinductiveHypothesisSet(bindings={var1: {Term.zero()}, var2: set()},
                                             prohibited={var1: {var2}, var2: {var1}}),)
        self.assertEqual(expected, actual)

    def test_var_var_partiallybound2_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(bindings={var2: {Term.zero()}, var1: set()})
        actual = chs.constructive_disunification(var1, var2)
        expected = (CoinductiveHypothesisSet(bindings={var1: set(), var2: {Term.zero()}},
                                             prohibited={var1: {var2}, var2: {var1}}),)
        self.assertEqual(expected, actual)

    def test_var_var_prohibited_error(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(prohibited={var1: {Term.zero()}, var2: {Term.one()}})
        with self.assertRaises(Exception):
            chs.constructive_disunification(var1, var2)

    def test_var_var_prohibitedpartially1_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(prohibited={var1: {Term.zero()}, var2: set()})
        actual = chs.constructive_disunification(var1, var2)
        expected = (CoinductiveHypothesisSet(prohibited={var1: {Term.zero(), var2}, var2: {var1}}),)
        self.assertEqual(expected, actual)

    def test_var_var_prohibitedpartially2_positive(self):
        var1 = Variable('A')
        var2 = Variable('B')

        chs = CoinductiveHypothesisSet(prohibited={var2: {Term.zero()}, var1: set()})
        actual = chs.constructive_disunification(var1, var2)
        expected = (CoinductiveHypothesisSet(prohibited={var1: {var2}, var2: {Term.zero(), var1}}),)
        self.assertEqual(expected, actual, msg="\nExpected: {}\nActual:  {}\n".format(expected, actual))

    def test_compound_compound_negative(self):
        fun1 = Function('a', (Term.one(), Function('b', (Function('c'), Term.zero()))))
        fun2 = Function('a', (Term.one(), Function('b', (Function('c'), Term.zero()))))

        chs = CoinductiveHypothesisSet()
        actual = chs.constructive_disunification(fun1, fun2)
        expected = ()
        self.assertEqual(expected, actual)

    def test_compound_compound_non_deterministic_positive(self):
        fun1 = Function('a', (Variable('X'), Variable('Y')))
        fun2 = Function('a', (Term.zero(), Term.one()))

        chs = CoinductiveHypothesisSet()
        actual = chs.constructive_disunification(fun1, fun2)
        expected = (CoinductiveHypothesisSet(prohibited={Variable('X'): {Term.zero()}}),
                    CoinductiveHypothesisSet(prohibited={Variable('Y'): {Term.one()}}),)
        self.assertEqual(expected, actual)

    def test_compound_compound_with_var_positive(self):
        var = Variable('A')
        fun1 = Function('a', (Term.one(), Function('b', (Function('c'), Term.zero()))))
        fun2 = Function('a', (Term.one(), Function('b', (var, Term.zero()))))

        chs = CoinductiveHypothesisSet()
        actual = chs.constructive_disunification(fun1, fun2)
        expected = (CoinductiveHypothesisSet(prohibited={var: {Function('c')}}),)
        self.assertEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}\n".format(expected, actual))

    def test_compound_compound_mismatching_args_positive(self):
        fun1 = Function('a', (Term.one(),))
        fun2 = Function('a', (Term.zero(),))

        chs = CoinductiveHypothesisSet()
        actual = chs.constructive_disunification(fun1, fun2)
        expected = (CoinductiveHypothesisSet(),)
        self.assertEqual(expected, actual)

    def test_compound_compound_mismatching_arity_positive(self):
        fun1 = Function('a', (Term.one(),))
        fun2 = Function('a', (Term.zero(), Term.one()))

        chs = CoinductiveHypothesisSet()
        actual = chs.constructive_disunification(fun1, fun2)
        expected = (CoinductiveHypothesisSet(),)
        self.assertEqual(expected, actual)

    def test_compound_compound_mismatching_name_positive(self):
        fun1 = Function('a', (Term.one(),))
        fun2 = Function('b', (Term.one(),))

        chs = CoinductiveHypothesisSet()
        actual = chs.constructive_disunification(fun1, fun2)
        expected = (CoinductiveHypothesisSet(),)
        self.assertEqual(expected, actual)

    def test_compound_compound_mismatching_var_positive(self):
        var = Variable('A')
        fun1 = Function('a', (var,))
        fun2 = Function('a', (Term.one(),))

        chs = CoinductiveHypothesisSet(prohibited={var: {Term.one()}})
        actual = chs.constructive_disunification(fun1, fun2)
        expected = (CoinductiveHypothesisSet(prohibited={var: {Term.one()}}),)
        self.assertEqual(expected, actual)

    def test_compound_compound_mismatching_var_reversed_positive(self):
        var = Variable('A')
        fun1 = Function('a', (Term.one(),))
        fun2 = Function('a', (var,))

        chs = CoinductiveHypothesisSet(prohibited={var: {Term.one()}})
        actual = chs.constructive_disunification(fun1, fun2)
        expected = (CoinductiveHypothesisSet(prohibited={var: {Term.one()}}),)
        self.assertEqual(expected, actual)

    def test_term_term_negative(self):
        term1 = Term.zero()
        term2 = Term.zero()

        chs = CoinductiveHypothesisSet()
        actual = chs.constructive_disunification(term1, term2)
        expected = ()
        self.assertEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}\n".format(expected, actual))

    def test_term_term_positive(self):
        term1 = Term.zero()
        term2 = Term.one()

        chs = CoinductiveHypothesisSet()
        actual = chs.constructive_disunification(term1, term2)
        expected = (CoinductiveHypothesisSet(),)
        self.assertEqual(expected, actual)


# noinspection DuplicatedCode
class TestPropagateLiteralDownToRule(unittest.TestCase):

    def setUp(self) -> None:
        CoinductiveHypothesisSet.unused_var_int = 0

    def test_empty1(self):
        r1 = NormalRule(p, (q,))

        chs = CoinductiveHypothesisSet()
        actual = chs.propagate_literal_down_to_rule(p, r1)
        expected = CoinductiveHypothesisSet()
        self.assertEqual(expected, actual)

    def test_empty2(self):
        r1 = NormalRule(p)
        chs = CoinductiveHypothesisSet()
        actual = chs.propagate_literal_down_to_rule(p, r1)
        expected = CoinductiveHypothesisSet()
        self.assertEqual(expected, actual)

    def test_prop(self):
        r1 = NormalRule(p, (q,))

        chs = CoinductiveHypothesisSet({q_X}, {X: {Term.one()}}, {X: {Term.zero()}})
        actual = chs.propagate_literal_down_to_rule(p, r1)
        expected = CoinductiveHypothesisSet({q_X}, {X: {Term.one()}}, {X: {Term.zero()}})
        self.assertEqual(expected, actual)

    def test_bound_vars1(self):
        r1 = NormalRule(p_X, (q,))
        chs = CoinductiveHypothesisSet()
        actual = chs.propagate_literal_down_to_rule(p_1, r1)
        expected = CoinductiveHypothesisSet(set(), {X: {Term.one()}}, {X: set()})
        self.assertEqual(expected, actual, msg="\nExpected: {}\n  Actual: {}\n".format(expected, actual))

    def test_bound_vars2(self):
        r1 = NormalRule(p_X, (q,))
        chs = CoinductiveHypothesisSet(literals={a_X}, bindings=defaultdict(set, {X: {Term.zero()}}))
        actual = chs.propagate_literal_down_to_rule(p_1, r1)
        expected = CoinductiveHypothesisSet(literals={a_F0}, bindings={X: {Term.one()}, F0: {Term.zero()}},
                                            prohibited={X: set()})
        self.assertEqual(expected, actual)

    def test_prohibited_vars1(self):
        r1 = NormalRule(p_X, (q,))
        chs = CoinductiveHypothesisSet(literals={a_X}, prohibited=defaultdict(set, {X: {Term.one()}}))
        actual = chs.propagate_literal_down_to_rule(p_1, r1)
        expected = CoinductiveHypothesisSet(literals={a_F0}, bindings=defaultdict(set, {X: {Term.one()}}),
                                            prohibited=defaultdict(set, {F0: {Term.one()}, X: set()}))
        self.assertEqual(expected, actual)

    def test_body_vars1(self):
        r1 = NormalRule(p_Y, (a_X,))
        chs = CoinductiveHypothesisSet(literals={a_X})
        actual = chs.propagate_literal_down_to_rule(p_Y, r1)
        expected = CoinductiveHypothesisSet(literals={a_F0})
        self.assertEqual(expected, actual)

    def test_body_vars2(self):
        r1 = NormalRule(p_Y, (a_F0,))
        chs = CoinductiveHypothesisSet(literals={a_F0})
        actual = chs.propagate_literal_down_to_rule(p_Y, r1)
        expected = CoinductiveHypothesisSet(literals={a_F1})
        self.assertEqual(expected, actual)


# noinspection DuplicatedCode
class TestPropagateRuleDownToLiteral(unittest.TestCase):

    def setUp(self) -> None:
        CoinductiveHypothesisSet.unused_var_int = 0

    def test_empty1(self):
        r1 = NormalRule(p, (q,))
        chs = CoinductiveHypothesisSet()
        actual = chs.propagate_rule_down_to_literal(r1, 0, q)
        expected = CoinductiveHypothesisSet()
        self.assertEqual(expected, actual)

    def test_empty2(self):
        r1 = NormalRule(p_X, (q_X, q_Y))
        chs = CoinductiveHypothesisSet()
        actual = chs.propagate_rule_down_to_literal(r1, 0, q_F0)
        expected = CoinductiveHypothesisSet()
        self.assertEqual(expected, actual)

    def test_prop(self):
        r1 = NormalRule(p, (q,))
        chs = CoinductiveHypothesisSet({q_X}, {X: {Term.zero()}}, {X: {Term.one()}})
        actual = chs.propagate_rule_down_to_literal(r1, 0, q)
        expected = CoinductiveHypothesisSet({q_X}, {X: {Term.zero()}}, {X: {Term.one()}})
        self.assertEqual(expected, actual)

    def test_bound_vars1(self):
        r1 = NormalRule(p_X, (q_X,))
        chs = CoinductiveHypothesisSet({a_X}, {X: {Term.zero()}}, {X: {Term.one()}})
        actual = chs.propagate_rule_down_to_literal(r1, 0, q_F0)
        expected = CoinductiveHypothesisSet({a_F0}, {F0: {Term.zero()}}, {F0: {Term.one()}})
        self.assertEqual(expected, actual)

    def test_bound_vars2(self):
        r1 = NormalRule(p_X, (q,))
        chs = CoinductiveHypothesisSet(literals={a_X}, bindings=defaultdict(set, {X: {Term.zero()}}))
        actual = chs.propagate_rule_down_to_literal(r1, 0, q)
        expected = CoinductiveHypothesisSet(literals={a_X}, bindings=defaultdict(set, {X: {Term.zero()}}))
        self.assertEqual(expected, actual)

    def test_bound_vars3_1(self):
        r1 = NormalRule(p_X, (q_X, q_Y,))
        chs = CoinductiveHypothesisSet(literals={a_X}, bindings=defaultdict(set, {X: {Term.zero()}, Y: {Term.one()}}))
        actual = chs.propagate_rule_down_to_literal(r1, 0, q_F0)
        expected = CoinductiveHypothesisSet(literals={a_F0},
                                            bindings=defaultdict(set, {F0: {Term.zero()}}))
        self.assertEqual(expected, actual)

    def test_bound_vars3_2(self):
        r1 = NormalRule(p_X, (q_X, q_Y,))
        chs = CoinductiveHypothesisSet(literals={a_X}, bindings=defaultdict(set, {X: {Term.zero()}, Y: {Term.one()}}))
        actual = chs.propagate_rule_down_to_literal(r1, 1, q_F0)
        expected = CoinductiveHypothesisSet(literals={a_X},
                                            bindings=defaultdict(set, {X: {Term.zero()}, F0: {Term.one()}}))
        self.assertEqual(expected, actual)

    def test_bound_vars4(self):
        r1 = NormalRule(p_X, (q_X, q_Y,))
        chs = CoinductiveHypothesisSet(bindings=defaultdict(set, {X: {Term.zero()}, Y: {Term.one()}}))
        actual = chs.propagate_rule_down_to_literal(r1, 1, q_F0)
        expected = CoinductiveHypothesisSet(bindings=defaultdict(set, {F0: {Term.one()}}))
        self.assertEqual(expected, actual)

    def test_body_vars1(self):
        r1 = NormalRule(p_X, (q_Y,))
        chs = CoinductiveHypothesisSet(bindings=defaultdict(set, {X: {Term.one()}, Y: {Term.zero()}}),
                                       prohibited=defaultdict(set, {X: {Term.zero()}, Y: {Term.one()}}))
        actual = chs.propagate_rule_down_to_literal(r1, 0, q_F0)
        expected = CoinductiveHypothesisSet(bindings=defaultdict(set, {F0: {Term.zero()}}),
                                            prohibited=defaultdict(set, {F0: {Term.one()}}))
        self.assertEqual(expected, actual)

    def test_compound(self):
        r1 = NormalRule(p, (BasicLiteral.make_literal('a', 1, Function('b', (X, Y))),))
        lit = BasicLiteral.make_literal('a', 1, Function('b', (F0, F1)))
        chs = CoinductiveHypothesisSet(bindings=defaultdict(set, {X: {Term.one()}, Y: {Term.zero()}}),
                                       prohibited=defaultdict(set, {X: {Term.zero()}, Y: {Term.one()}}))
        actual = chs.propagate_rule_down_to_literal(r1, 0, lit)
        expected = CoinductiveHypothesisSet(bindings=defaultdict(set, {F0: {Term.one()}, F1: {Term.zero()}}),
                                            prohibited=defaultdict(set, {F0: {Term.zero()}, F1: {Term.one()}}))
        self.assertEqual(expected, actual)


# noinspection DuplicatedCode
class TestPropagateLiteralUpToRule(unittest.TestCase):

    def setUp(self) -> None:
        CoinductiveHypothesisSet.unused_var_int = 0

    def test_empty1(self):
        r1 = NormalRule(p, (q,))
        chs = CoinductiveHypothesisSet()
        actual = chs.propagate_literal_up_to_rule(q, 0, r1)
        expected = CoinductiveHypothesisSet()
        self.assertEqual(expected, actual)

    def test_empty2(self):
        r1 = NormalRule(p_X, (q_X, q_Y))
        chs = CoinductiveHypothesisSet()
        actual = chs.propagate_literal_up_to_rule(q_F0, 0, r1)
        expected = CoinductiveHypothesisSet()
        self.assertEqual(expected, actual)

    def test_prop(self):
        r1 = NormalRule(p, (q, a))
        chs = CoinductiveHypothesisSet({a_X}, {X: {Term.one()}}, {X: {Term.zero()}})
        actual = chs.propagate_literal_up_to_rule(a, 1, r1)
        expected = CoinductiveHypothesisSet({a_X}, {X: {Term.one()}}, {X: {Term.zero()}})
        self.assertEqual(expected, actual)

    def test_bound_vars1(self):
        r1 = NormalRule(p_X, (q_X,))
        chs = CoinductiveHypothesisSet(set(), {F0: {Term.one()}})
        actual = chs.propagate_literal_up_to_rule(q_F0, 0, r1)
        expected = CoinductiveHypothesisSet(set(), {X: {Term.one()}})
        self.assertEqual(expected, actual)

    def test_body_vars(self):
        r1 = NormalRule(p_X, (q_Y,))
        chs = CoinductiveHypothesisSet({a_X}, {X: {Term.zero()}, F0: {Term.one()}})
        actual = chs.propagate_literal_up_to_rule(q_F0, 0, r1)
        expected = CoinductiveHypothesisSet({a_X}, {X: {Term.zero()}, Y: {Term.one()}})
        self.assertEqual(expected, actual)
