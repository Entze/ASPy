import clingo.ast

from aspy.Atom import Atom
from aspy.ClauseElement import ClauseElement, HeadClauseElement
from aspy.Comparison import Comparison, ComparisonOperator
from aspy.Directive import Directive
from aspy.Literal import BasicLiteral, Sign
from aspy.Program import Program
from aspy.Rule import NormalRule, Rule, Goal, IntegrityConstraint
from aspy.Symbol import Symbol, TopLevelSymbol, Function, Term, StringConstant, IntegerConstant, Variable


def _typecheck(obj, should):
    assert obj.ast_type is should, "clingo.ast.AST {} should have type {}, but has type {}.".format(
        obj, should, obj.ast_type)


def _from_clingo_ast_boolean_constant(ast: clingo.ast.AST) -> Directive:
    _typecheck(ast, clingo.ast.ASTType.BooleanConstant)
    if ast.value is True:
        return Directive.true()
    else:
        assert ast.value is False
        return Directive.false()


def _from_clingo_ast_variable(ast: clingo.ast.AST) -> Variable:
    _typecheck(ast, clingo.ast.ASTType.Variable)
    return Variable(ast.name)


def _from_clingo_ast_symbolic_term(ast: clingo.ast.AST) -> Term:
    assert ast.ast_type is clingo.ast.ASTType.SymbolicTerm, "clingo.ast.AST {} should have type {}, but has type {}.".format(
        ast, clingo.ast.ASTType.SymbolicTerm, ast.ast_type)
    if ast.symbol.type is clingo.SymbolType.String:
        return Term(StringConstant(ast.symbol.string))
    elif ast.symbol.type is clingo.SymbolType.Number:
        return Term(IntegerConstant(ast.symbol.number))
    else:
        raise NotImplementedError("Term {} unhandled clingo.SymbolType {}.".format(ast.symbol, ast.symbol.type))


def _from_clingo_ast_function(ast: clingo.ast.AST) -> Function:
    _typecheck(ast, clingo.ast.ASTType.Function)
    name = ast.name
    arguments = tuple(_from_clingo_ast_symbol(argument) for argument in ast.arguments)
    return Function(name, arguments)


def _from_clingo_ast_symbol(ast: clingo.ast.AST) -> Symbol:
    if ast.ast_type is clingo.ast.ASTType.Function:
        return _from_clingo_ast_function(ast)
    elif ast.ast_type is clingo.ast.ASTType.SymbolicTerm:
        return _from_clingo_ast_symbolic_term(ast)
    elif ast.ast_type is clingo.ast.ASTType.Variable:
        return _from_clingo_ast_variable(ast)


def _from_clingo_ast_toplevelsymbol(ast: clingo.ast.AST) -> TopLevelSymbol:
    _typecheck(ast, clingo.ast.ASTType.Function)
    return _from_clingo_ast_function(ast)


def _from_clingo_ast_symbolic_atom(ast: clingo.ast.AST) -> Atom:
    _typecheck(ast, clingo.ast.ASTType.SymbolicAtom)
    return Atom(_from_clingo_ast_toplevelsymbol(ast.symbol))


def _from_clingo_ast_literal(ast: clingo.ast.AST) -> BasicLiteral:
    assert ast.ast_type is clingo.ast.ASTType.Literal, "clingo.ast.AST {} should have type {}, but has type {}.".format(
        ast, clingo.ast.ASTType.Literal, ast.ast_type)
    sign = Sign(ast.sign)
    atom = _from_clingo_ast_symbolic_atom(ast.atom)
    return BasicLiteral(sign=sign, atom=atom)


def _from_clingo_ast_comparison(ast: clingo.ast.AST) -> Comparison:
    left = _from_clingo_ast_symbol(ast.left)
    op = ComparisonOperator(ast.comparison)
    right = _from_clingo_ast_symbol(ast.right)
    return Comparison(left, op, right)


def _from_clingo_ast_head_clause_element(ast: clingo.ast.AST) -> HeadClauseElement:
    if ast.ast_type is clingo.ast.ASTType.Literal:
        if ast.atom.ast_type is clingo.ast.ASTType.SymbolicAtom:
            return _from_clingo_ast_literal(ast)
        elif ast.atom.ast_type is clingo.ast.ASTType.BooleanConstant:
            return _from_clingo_ast_boolean_constant(ast)


def _from_clingo_ast_clause_element(ast: clingo.ast.AST) -> ClauseElement:
    if ast.ast_type is clingo.ast.ASTType.Literal:
        if ast.atom.ast_type is clingo.ast.ASTType.Comparison:
            return _from_clingo_ast_comparison(ast.atom)
        return _from_clingo_ast_head_clause_element(ast)


def _from_clingo_ast_normal_rule(ast: clingo.ast.AST) -> NormalRule:
    head = _from_clingo_ast_literal(ast.head)
    body = tuple(_from_clingo_ast_clause_element(element) for element in ast.body)
    return NormalRule(head, body)


def _from_clingo_ast_goal(ast: clingo.ast.AST) -> Goal:
    pass


def _from_clingo_ast_integrity_constraint(ast: clingo.ast.AST) -> IntegrityConstraint:
    body = tuple(_from_clingo_ast_clause_element(element) for element in ast.body)
    return IntegrityConstraint(body)


def _from_clingo_ast_rule(ast: clingo.ast.AST) -> Rule:
    _typecheck(ast, clingo.ast.ASTType.Rule)
    if ast.head.ast_type is clingo.ast.ASTType.Literal:
        if ast.head.atom.ast_type is clingo.ast.ASTType.SymbolicAtom:
            # Normal Rule
            return _from_clingo_ast_normal_rule(ast)
        elif ast.head.atom.ast_type is clingo.ast.ASTType.BooleanConstant:
            if ast.head.atom.value is True:
                # Goal
                return _from_clingo_ast_goal(ast)
            else:
                return _from_clingo_ast_integrity_constraint(ast)


def from_clingo_ast(ast: clingo.ast.AST):
    if ast.ast_type is clingo.ast.ASTType.Rule:
        return _from_clingo_ast_rule(ast)


def from_string(program: str) -> Program:
    statements = []
    clingo.ast.parse_string(program, callback=lambda ast: statements.append(from_clingo_ast(ast)))
    return Program(tuple(statement for statement in statements if statement is not None))
