/*****************************************************
 *  Implementation of "WhileStmt".
 *
 *  Please refer to ast/ast.hpp for the definition.
 *
 *  Keltin Leung 
 */

#include "ast/ast.hpp"
#include "ast/visitor.hpp"
#include "config.hpp"

using namespace mind;
using namespace mind::ast;

/* Creates a new WhileStmt node.
 *
 * PARAMETERS:
 *   cond    - the test expression
 *   body    - the loop body
 *   l       - position in the source text
 */
WhileStmt::WhileStmt(Expr *cond, Statement *body, Location *l) {

    setBasicInfo(WHILE_STMT, l);

    condition = cond;
    loop_body = body;
}

/* Visits the current node.
 *
 * PARAMETERS:
 *   v       - the visitor
 */
void WhileStmt::accept(Visitor *v) { v->visit(this); }

/* Prints the current AST node.
 *
 * PARAMETERS:
 *   os      - the output stream
 */
void WhileStmt::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    newLine(os);
    os << condition;

    newLine(os);
    os << loop_body << ")";
    decIndent(os);
}

/* Creates a new BreakStmt node.
 *
 * PARAMETERS:
 *   l       - position in the source text
 */
BreakStmt::BreakStmt(Location *l) { setBasicInfo(BREAK_STMT, l); }

/* Visits the current node.
 *
 * PARAMETERS:
 *   v       - the visitor
 */
void BreakStmt::accept(Visitor *v) { v->visit(this); }

/* Prints the current AST node.
 *
 * PARAMETERS:
 *   os      - the output stream
 */
void BreakStmt::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    newLine(os);
    decIndent(os);
}


ContStmt::ContStmt(Location *l) { setBasicInfo(CONT_STMT, l); }

void ContStmt::accept(Visitor *v) { v->visit(this); }

void ContStmt::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    newLine(os);
    decIndent(os);
}


ForStmt::ForStmt(Statement *init, Expr *cond, Expr *update, Statement *loop_body,
            Location *l): init(init), condition(cond), update(update), loop_body(loop_body) {
    setBasicInfo(FOR_STMT, l);
            }

void ForStmt::accept(Visitor *v) { v->visit(this); }

void ForStmt::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    newLine(os);
    os << init;
    newLine(os);
    os << condition;
    newLine(os);
    os << update;
    newLine(os);
    os << loop_body << ")";
    decIndent(os);
}

DoWhileStmt::DoWhileStmt(Statement *loop_body, Expr *cond, Location *l): loop_body(loop_body),condition(cond) {
    setBasicInfo(DO_WHILE_STMT, l);
}

void DoWhileStmt::accept(Visitor *v) { v->visit(this); }

void DoWhileStmt::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    newLine(os);
    os << loop_body;
    newLine(os);
    os << condition << ")";
    decIndent(os);
}