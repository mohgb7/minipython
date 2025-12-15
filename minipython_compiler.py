#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
minipython_compiler.py
Version complète :
- parsing Lark (minipython subset)
- AST transformer
- vérification sémantique (déclarations)
- génération TAC (tuple + texte)
- exécution minimale du TAC
- main(argv) pour exécuter un fichier .minipython
"""

import sys
from lark import Lark, Transformer
from anytree import Node, RenderTree
from anytree.exporter import DotExporter

# -------------------------
# 1) Grammaire (simplifiée mais conforme au TP)
# -------------------------
grammar = r"""
start: decl* stmt*

decl: type var_list ";"
var_list: var_item ("," var_item)*
var_item: CNAME ("[" NUMBER ("]" "[" NUMBER "]")* "]")?    -> var_item

type: "int" | "float" | "bool" | "string"

stmt: assign
    | print_stmt
    | if_stmt
    | while_stmt
    | proc_def
    | proc_call ";"

assign: CNAME "=" expr ";"

print_stmt: "print" "(" expr ")" ";"

if_stmt: "if" "(" expr ")" "{" stmt* "}" ("else" "{" stmt* "}")?

while_stmt: "while" "(" expr ")" "{" stmt* "}"

proc_def: "def" "procedure" CNAME "(" [param_list] ")" "{" stmt* "return" "}"

param_list: param ("," param)*
param: type CNAME

proc_call: CNAME "(" [arg_list] ")"
arg_list: expr ("," expr)*

?expr: expr "&&" expr   -> and_
     | expr "||" expr   -> or_
     | "!" expr         -> not_
     | expr "==" expr   -> eq
     | expr "!=" expr   -> neq
     | expr "<" expr    -> lt
     | expr ">" expr    -> gt
     | expr "+" expr    -> add
     | expr "-" expr    -> sub
     | expr "*" expr    -> mul
     | expr "/" expr    -> div
     | "(" expr ")"
     | NUMBER            -> number
     | STRING            -> string
     | CNAME             -> var

COMMENT: "/*" /(.|\n)*?/ "*/"
%import common.CNAME
%import common.NUMBER
%import common.WS
%import common.ESCAPED_STRING -> STRING
%ignore WS
%ignore COMMENT
"""

parser = Lark(grammar, start="start", parser="lalr")

# -------------------------
# 2) Transformer -> AST
# -------------------------
class SyntaxTransformer(Transformer):
    def start(self, items):
        # items contains decl* stmt*
        return items

    def decl(self, items):
        # items: [type, var_list]
        t = str(items[0])
        vars_ = items[1]  # var_list returns list of var_item
        # var_item already returns name or name with indices (we keep as string)
        return ("decl", t, vars_)

    def var_list(self, items):
        return items

    def var_item(self, items):
        # token sequence: CNAME maybe NUMBERs; simplest: return string representation
        # Lark passes tokens; first is name
        name = str(items[0])
        # if more items exist (arrays) we ignore shape for this TP simplified version
        return name

    def assign(self, items):
        return ("assign", str(items[0]), items[1])

    def print_stmt(self, items):
        return ("print", items[0])

    def if_stmt(self, items):
        # items: cond, if_body (list), maybe else_body
        cond = items[0]
        # Lark gives following items as lists of Tree objects, but Transformer has already converted them
        # Extract bodies: items[1] ... items[n]
        # We will collect statements by inspecting types
        # In practice, items[1] is a list (from stmt*)
        if_body = []
        else_body = []
        # items[1:] are the statement bodies
        idx = 1
        while idx < len(items):
            stmt = items[idx]
            if isinstance(stmt, list):
                if_body.extend(stmt)
                idx += 1
            else:
                if_body.append(stmt)
                idx += 1
        # Check if there's an else clause (next items)
        if len(items) > idx:
            while idx < len(items):
                else_body.append(items[idx])
                idx += 1
        return ("if", cond, if_body, else_body)

    def while_stmt(self, items):
        cond = items[0]
        body = items[1] if len(items) > 1 else []
        return ("while", cond, body)

    def proc_def(self, items):
        # items: NAME, params?, body..., 'return' is omitted by grammar
        name = str(items[0])
        # find params (if present) and body
        # if items[1] is list of params, else may be body
        idx = 1
        params = []
        if idx < len(items) and isinstance(items[idx], list) and items[idx] and isinstance(items[idx][0], tuple) and items[idx][0][0] == "param":
            params = items[idx]
            idx += 1
        body = []
        while idx < len(items):
            body.append(items[idx])
            idx += 1
        return ("proc", name, params, body)

    def param_list(self, items):
        return items

    def param(self, items):
        # items: type, name
        return ("param", str(items[0]), str(items[1]))

    def proc_call(self, items):
        name = str(items[0])
        args = items[1] if len(items) > 1 else []
        return ("call", name, args)

    def arg_list(self, items):
        return items

    # Expressions
    def number(self, n):
        return int(n[0])

    def string(self, s):
        return str(s[0][1:-1])

    def var(self, name):
        return str(name[0])

    def add(self, items): return ("+", items[0], items[1])
    def sub(self, items): return ("-", items[0], items[1])
    def mul(self, items): return ("*", items[0], items[1])
    def div(self, items): return ("/", items[0], items[1])
    def eq(self, items): return ("==", items[0], items[1])
    def neq(self, items): return ("!=", items[0], items[1])
    def lt(self, items): return ("<", items[0], items[1])
    def gt(self, items): return (">", items[0], items[1])
    def and_(self, items): return ("&&", items[0], items[1])
    def or_(self, items): return ("||", items[0], items[1])
    def not_(self, items): return ("!", items[0])

# -------------------------
# 3) Vérification sémantique simple (déclarations)
# -------------------------
# Helper function to convert Lark Trees to tuples
def tree_to_tuple(obj):
    """Convert Lark Tree objects to tuples recursively"""
    if isinstance(obj, tuple):
        return tuple(tree_to_tuple(item) for item in obj)
    elif isinstance(obj, list):
        return [tree_to_tuple(item) for item in obj]
    elif hasattr(obj, 'data'):  # It's a Lark Tree
        # Tree with .data attribute
        children = [tree_to_tuple(child) for child in obj.children]
        return (obj.data,) + tuple(children)
    else:
        return obj

class SemanticChecker:
    def __init__(self):
        self.symtable = {}   # var -> type
        self.procs = {}      # name -> (params, body)

    def check(self, ast_list):
        # ast_list is list of statements (tuples or Lark Trees)
        # First convert any remaining Trees to tuples
        ast_list = [tree_to_tuple(stmt) for stmt in ast_list]
        new_ast = []
        for stmt in ast_list:
            kind = stmt[0]
            if kind == "decl":
                t = stmt[1]
                names = stmt[2]
                for name in names:
                    if name in self.symtable:
                        raise Exception(f"Variable '{name}' déjà déclarée")
                    self.symtable[name] = t
                new_ast.append(stmt)
            elif kind == "proc":
                name = stmt[1]
                params = stmt[2]
                if name in self.procs:
                    raise Exception(f"Procédure '{name}' déjà définie")
                self.procs[name] = (params, stmt[3])
                new_ast.append(stmt)
            else:
                # for assign/print/if/while/call accept for now; deeper checks could be added
                new_ast.append(stmt)
        return new_ast

# -------------------------
# 4) Construction AnyTree pour affichage AST
# -------------------------
def build_anytree(node, parent=None):
    if isinstance(node, tuple):
        label = str(node[0])
        n = Node(label, parent=parent)
        for child in node[1:]:
            build_anytree(child, n)
        return n
    elif isinstance(node, list):
        n = Node("list", parent=parent)
        for item in node:
            build_anytree(item, n)
        return n
    else:
        Node(str(node), parent=parent)
        return parent

# -------------------------
# 5) Générateur TAC (tuple + texte) complet
# -------------------------
class TACGenerator:
    def __init__(self):
        self.tac = []         # list of tuples ("OP", args...)
        self.text = []        # textual lines
        self.temp_counter = 0
        self.label_counter = 0

    def new_temp(self):
        t = f"t{self.temp_counter}"
        self.temp_counter += 1
        return t

    def new_label(self, base="L"):
        l = f"{base}{self.label_counter}"
        self.label_counter += 1
        return l

    def emit(self, *instr):
        self.tac.append(instr)
        # textual representation
        op = instr[0]
        args = instr[1:]
        if op == "DECLARE":
            self.text.append(f"DECLARE {args[0]}")
        elif op == "LOAD":
            self.text.append(f"LOAD {args[0]} -> {args[1]}")
        elif op in ("ADD","SUB","MUL","DIV","AND","OR","EQ","NE","LT","GT"):
            self.text.append(f"{op} {args[0]}, {args[1]} -> {args[2]}")
        elif op == "STORE":
            self.text.append(f"STORE {args[0]} -> {args[1]}")
        elif op == "PRINT":
            self.text.append(f"PRINT {args[0]}")
        elif op == "JMP":
            self.text.append(f"JMP {args[0]}")
        elif op == "JMP_IF_FALSE":
            self.text.append(f"JMP_IF_FALSE {args[0]} -> {args[1]}")
        elif op == "LABEL":
            self.text.append(f"{args[0]}:")
        elif op == "CALL":
            self.text.append(f"CALL {args[0]}")
        elif op == "RETURN":
            self.text.append("RETURN")
        else:
            self.text.append(" ".join(map(str, instr)))

    def generate(self, ast):
        # ast may be list of statements
        if isinstance(ast, list):
            for s in ast:
                self.generate(s)
        elif isinstance(ast, tuple):
            kind = ast[0]
            if kind == "decl":
                for name in ast[2]:
                    self.emit("DECLARE", name)
            elif kind == "assign":
                name = ast[1]
                expr = ast[2]
                r = self.gen_expr(expr)
                # r is either immediate (int/string) or temp or var
                # STORE r into name
                if isinstance(r, (int, str)) and not r.startswith("t"):
                    # if r is a var name or literal: LOAD it into temp then store
                    tmp = self.new_temp()
                    self.emit("LOAD", r, tmp)
                    self.emit("STORE", tmp, name)
                else:
                    self.emit("STORE", r, name)
            elif kind == "print":
                expr = ast[1]
                r = self.gen_expr(expr)
                # if r is not temp (literal or var) we make sure to LOAD
                if isinstance(r, int) or (isinstance(r, str) and not r.startswith("t")):
                    tmp = self.new_temp()
                    self.emit("LOAD", r, tmp)
                    self.emit("PRINT", tmp)
                else:
                    self.emit("PRINT", r)
            elif kind == "if":
                cond = ast[1]
                if_body = ast[2] or []
                else_body = ast[3] or []
                cond_temp = self.gen_expr(cond)
                # ensure cond_temp is a temp
                if not (isinstance(cond_temp, str) and cond_temp.startswith("t")):
                    tmpc = self.new_temp()
                    self.emit("LOAD", cond_temp, tmpc)
                    cond_temp = tmpc
                else:
                    pass
                label_else = self.new_label("ELSE")
                label_end = self.new_label("ENDIF")
                # if false jump to else
                self.emit("JMP_IF_FALSE", cond_temp, label_else)
                # if body
                self.generate(if_body)
                self.emit("JMP", label_end)
                # else label
                self.emit("LABEL", label_else)
                # else body
                self.generate(else_body)
                # end
                self.emit("LABEL", label_end)
            elif kind == "while":
                cond = ast[1]
                body = ast[2] or []
                label_start = self.new_label("WHILE_START")
                label_end = self.new_label("WHILE_END")
                self.emit("LABEL", label_start)
                cond_temp = self.gen_expr(cond)
                if not (isinstance(cond_temp, str) and cond_temp.startswith("t")):
                    tmpc = self.new_temp()
                    self.emit("LOAD", cond_temp, tmpc)
                    cond_temp = tmpc
                self.emit("JMP_IF_FALSE", cond_temp, label_end)
                self.generate(body)
                self.emit("JMP", label_start)
                self.emit("LABEL", label_end)
            elif kind == "proc":
                name = ast[1]
                params = ast[2]
                body = ast[3]
                # mark label for procedure
                self.emit("LABEL", f"PROC_{name}")
                # body
                self.generate(body)
                self.emit("RETURN")
            elif kind == "call":
                name = ast[1]
                args = ast[2]
                # push args (implementation simple: generate args then CALL)
                for a in args:
                    r = self.gen_expr(a)
                    # ensure r in temp
                    if isinstance(r, int) or (isinstance(r, str) and not r.startswith("t")):
                        ta = self.new_temp()
                        self.emit("LOAD", r, ta)
                        self.emit("STORE", ta, f"@arg")  # simplified arg passing
                    else:
                        self.emit("STORE", r, f"@arg")
                self.emit("CALL", name)
            else:
                # unknown
                pass

    def gen_expr(self, expr):
        # Returns either literal (int/str), variable name, or temp name
        if isinstance(expr, int):
            tmp = self.new_temp()
            self.emit("LOAD", expr, tmp)
            return tmp
        if isinstance(expr, str):
            # could be variable or string literal
            # in our AST strings are plain Python str (for string literals) OR var names
            # We treat string literals as distinct: they came from grammar STRING, while var names from var()
            # For simplicity: if expr is quoted? We earlier stripped quotes, so treat as var unless not identifier
            if expr.isidentifier():
                # load variable into temp when needed later
                return expr
            else:
                # treat as literal string
                tmp = self.new_temp()
                self.emit("LOAD", expr, tmp)
                return tmp
        if isinstance(expr, tuple):
            op = expr[0]
            if op in ("+", "-", "*", "/"):
                left = self.gen_expr(expr[1])
                right = self.gen_expr(expr[2])
                # ensure left/right are temps
                if not (isinstance(left, str) and left.startswith("t")):
                    tleft = self.new_temp()
                    self.emit("LOAD", left, tleft)
                    left = tleft
                if not (isinstance(right, str) and right.startswith("t")):
                    tright = self.new_temp()
                    self.emit("LOAD", right, tright)
                    right = tright
                tres = self.new_temp()
                mapop = {"+":"ADD","-":"SUB","*":"MUL","/":"DIV"}
                self.emit(mapop[op], left, right, tres)
                return tres
            elif op in ("==","!=","<",">"):
                left = self.gen_expr(expr[1])
                right = self.gen_expr(expr[2])
                if not (isinstance(left, str) and left.startswith("t")):
                    tleft = self.new_temp()
                    self.emit("LOAD", left, tleft)
                    left = tleft
                if not (isinstance(right, str) and right.startswith("t")):
                    tright = self.new_temp()
                    self.emit("LOAD", right, tright)
                    right = tright
                tres = self.new_temp()
                opmap = {"==":"EQ","!=":"NE","<":"LT",">":"GT"}
                self.emit(opmap[op], left, right, tres)
                return tres
            elif op in ("&&","||"):
                left = self.gen_expr(expr[1])
                right = self.gen_expr(expr[2])
                if not (isinstance(left, str) and left.startswith("t")):
                    tleft = self.new_temp()
                    self.emit("LOAD", left, tleft)
                    left = tleft
                if not (isinstance(right, str) and right.startswith("t")):
                    tright = self.new_temp()
                    self.emit("LOAD", right, tright)
                    right = tright
                tres = self.new_temp()
                opmap = {"&&":"AND","||":"OR"}
                self.emit(opmap[op], left, right, tres)
                return tres
            elif op == "!":
                inner = self.gen_expr(expr[1])
                if not (isinstance(inner, str) and inner.startswith("t")):
                    tin = self.new_temp()
                    self.emit("LOAD", inner, tin)
                    inner = tin
                tres = self.new_temp()
                # implement NOT as EQ inner,0 -> tres (simple)
                self.emit("EQ", inner, 0, tres)
                return tres
        # default
        return expr

# -------------------------
# 6) Exécuteur TAC minimal
# -------------------------
class TACExecutor:
    def __init__(self, tac):
        # tac: list of tuples
        self.tac = tac
        self.pc = 0
        self.env = {}      # variables and temps
        self.labels = {}   # label -> pc index
        self.preprocess_labels()

    def preprocess_labels(self):
        # collect labels positions
        for i, instr in enumerate(self.tac):
            if instr[0] == "LABEL":
                self.labels[instr[1]] = i

    def run(self):
        self.pc = 0
        while self.pc < len(self.tac):
            instr = self.tac[self.pc]
            op = instr[0]
            # print("EXEC:", instr)
            if op == "DECLARE":
                name = instr[1]
                self.env[name] = None
            elif op == "LOAD":
                value = instr[1]
                target = instr[2]
                # value can be literal int, string, or variable name
                if isinstance(value, int):
                    self.env[target] = value
                elif isinstance(value, str):
                    if value in self.env:
                        self.env[target] = self.env.get(value)
                    else:
                        # treat as string literal if not variable
                        self.env[target] = value
            elif op == "STORE":
                src = instr[1]
                dest = instr[2]
                val = self.env.get(src, None) if src in self.env else src
                # if src is literal int (rare), use it
                if isinstance(src, int):
                    val = src
                self.env[dest] = val
            elif op in ("ADD","SUB","MUL","DIV","AND","OR","EQ","NE","LT","GT"):
                a = instr[1]; b = instr[2]; res = instr[3]
                aval = self.env.get(a, a) if a in self.env else a
                bval = self.env.get(b, b) if b in self.env else b
                if op == "ADD": self.env[res] = aval + bval
                elif op == "SUB": self.env[res] = aval - bval
                elif op == "MUL": self.env[res] = aval * bval
                elif op == "DIV": self.env[res] = aval / bval
                elif op == "AND": self.env[res] = bool(aval) and bool(bval)
                elif op == "OR": self.env[res] = bool(aval) or bool(bval)
                elif op == "EQ": self.env[res] = 1 if aval == bval else 0
                elif op == "NE": self.env[res] = 1 if aval != bval else 0
                elif op == "LT": self.env[res] = 1 if aval < bval else 0
                elif op == "GT": self.env[res] = 1 if aval > bval else 0
            elif op == "PRINT":
                src = instr[1]
                val = self.env.get(src, src) if isinstance(src, str) else src
                print(val)
            elif op == "JMP":
                label = instr[1]
                self.pc = self.labels.get(label, self.pc)
                continue
            elif op == "JMP_IF_FALSE":
                cond = instr[1]; label = instr[2]
                val = self.env.get(cond, cond) if isinstance(cond, str) else cond
                if not bool(val):
                    self.pc = self.labels.get(label, self.pc)
                    continue
            elif op == "LABEL":
                pass
            elif op == "CALL":
                # simplified: jump to PROC_name label
                name = instr[1]
                target_label = f"PROC_{name}"
                if target_label in self.labels:
                    self.pc = self.labels[target_label]
                    continue
                else:
                    # no proc: ignore
                    pass
            elif op == "RETURN":
                # for simplicity, stop execution on RETURN
                return
            else:
                # unknown op
                pass
            self.pc += 1

# -------------------------
# 7) UTILITAIRES et MAIN
# -------------------------
class MiniPythonLarkAnalyzer:
    def __init__(self):
        self.parser = parser
        self.transformer = SyntaxTransformer()
        self.semantic_checker = None
        self.tac_generator = None

    def analyze(self, source):
        """Analyse lexicale, syntaxique et sémantique"""
        # 1. Lexical tokens
        print("\n=== Tokens ===")
        toks = list(self.parser.lex(source))
        for t in toks:
            print(" ", t)

        # 2. Parse + AST
        print("\n=== Parse Tree ===")
        tree = self.parser.parse(source)
        print(tree.pretty())

        print("\n=== AST Transformé ===")
        ast = self.transformer.transform(tree)
        print(ast)

        # 3. Semantic check
        print("\n=== Analyse sémantique ===")
        self.semantic_checker = SemanticChecker()
        ast_sem = self.semantic_checker.check(ast)
        print("Symtable:", self.semantic_checker.symtable)
        print("Procs:", self.semantic_checker.procs)

        # 4. Visualisation AST (AnyTree)
        print("\n=== Visualisation AST ===")
        root_node = build_anytree(("root", *ast_sem))
        for pre, _, node in RenderTree(root_node):
            print(f"{pre}{node.name}")
        try:
            DotExporter(root_node).to_picture("ast_minipython.png")
            print("AST exporté -> ast_minipython.png")
        except Exception as e:
            print("Export graphique indisponible:", type(e).__name__)

        return ast_sem

    def generate_tac(self, ast):
        """Génération du code intermédiaire TAC"""
        print("\n=== Génération TAC ===")
        self.tac_generator = TACGenerator()
        self.tac_generator.generate(ast)
        print("\n-- TAC (tuples) --")
        for t in self.tac_generator.tac:
            print(t)
        print("\n-- TAC (texte) --")
        for line in self.tac_generator.text:
            print(line)
        return self.tac_generator.tac

    def execute_tac(self, tac):
        """Exécution du TAC"""
        executor = TACExecutor(tac)
        executor.run()

def analyze_and_compile(source):
    # 1. Lexical tokens
    print("\n=== Tokens ===")
    toks = list(parser.lex(source))
    for t in toks:
        print(" ", t)

    # 2. Parse + AST
    print("\n=== Parse Tree ===")
    tree = parser.parse(source)
    print(tree.pretty())

    print("\n=== AST Transformé ===")
    ast = SyntaxTransformer().transform(tree)
    # ast is a list (start: decl* stmt*)
    print(ast)

    # 3. Semantic check
    print("\n=== Analyse sémantique ===")
    sem = SemanticChecker()
    ast_sem = sem.check(ast)
    print("Symtable:", sem.symtable)
    print("Procs:", sem.procs)

    # 4. Visualisation AST (AnyTree)
    print("\n=== Visualisation AST ===")
    root_node = build_anytree(("root", *ast_sem))
    for pre, _, node in RenderTree(root_node):
        print(f"{pre}{node.name}")
    try:
        DotExporter(root_node).to_picture("ast_minipython.png")
        print("AST exporté -> ast_minipython.png")
    except Exception as e:
        print("Export graphique indisponible:", type(e).__name__)

    # 5. TAC generation
    print("\n=== Génération TAC ===")
    gen = TACGenerator()
    gen.generate(ast_sem)
    print("\n-- TAC (tuples) --")
    for t in gen.tac:
        print(t)
    print("\n-- TAC (texte) --")
    for line in gen.text:
        print(line)

    return gen.tac

def main(argv=None):
    # Si aucun argument n'est passé à main(), on récupère sys.argv
    if argv is None:
        argv = sys.argv[1:]

    # Vérification : on doit passer exactement 1 fichier
    if len(argv) != 1:
        print("Usage : python minipython_compiler.py Monprogramme.minipython")
        return

    filename = argv[0]

    # Lecture du fichier source MiniPython
    try:
        with open(filename, "r") as f:
            code_source = f.read()
    except FileNotFoundError:
        print(f"Erreur : fichier '{filename}' introuvable.")
        return

    print("=" * 60)
    print(f"Analyse du fichier : {filename}")
    print("=" * 60)

    analyzer = MiniPythonLarkAnalyzer()

    # 1. Analyse complète (lexicale, syntaxique, sémantique)
    ast = analyzer.analyze(code_source)

    # 2. Génération TAC
    print("\n=== Code Intermédiaire (TAC) ===")
    tac = analyzer.generate_tac(ast)
    for instr in tac:
        print(instr)

    # 3. Exécution du TAC
    print("\n=== Exécution TAC ===")
    analyzer.execute_tac(tac)

    print("\n--- Fin d'exécution ---")


# Appel automatique du main()
if __name__ == "__main__":
    main()
