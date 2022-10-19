/*****************************************************
 *  Implementation of "VarRef".
 *
 *  Please refer to ast/ast.hpp for the definition.
 *
 *  Keltin Leung 
 *
 *  removed owner
 */

#include "ast/ast.hpp"
#include "ast/visitor.hpp"
#include "config.hpp"

using namespace mind;
using namespace mind::ast;

/* Creates a new VarRef node.
 *
 * PARAMETERS:
 *   n       - name of the referenced variable
 *   l       - position in the source text
 */
VarRef::VarRef(std::string n, Location *l) {

    setBasicInfo(VAR_REF, l);

    var = n;

    ATTR(sym) = NULL;
}


/* Visits the current node.
 *
 * PARAMETERS:
 *   v       - the visitor
 */
void VarRef::accept(Visitor *v) { v->visit(this); }

/* Prints the current AST node.
 *
 * PARAMETERS:
 *   os      - the output stream
 */
void VarRef::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    os << " " << '"' << var << '"';
    newLine(os);
    // if (NULL != owner)
    // os << owner << ")";
    // else
    // os << "())";
    decIndent(os);
}


ArrayRef::ArrayRef(Lvalue* arr_base, Expr * index, Location *l) {

    setBasicInfo(ARRAY_REF, l);

    this->arr_base = arr_base;
    this->index = index;
    ATTR(lv_kind) = MEM_VAR;
    
}

void ArrayRef::accept(Visitor *v) { v->visit(this); }

void ArrayRef::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    os << " some_var " << index;
    newLine(os);
    decIndent(os);
}