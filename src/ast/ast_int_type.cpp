/*****************************************************
 *  Implementation of "IntType".
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

/* Creates a new IntType node.
 *
 * PARAMETERS:
 *   l       - position in the source text
 */
IntType::IntType(Location *l) { setBasicInfo(INT_TYPE, l); }

/* Visits the current node.
 *
 * PARAMETERS:
 *   v       - the visitor
 */
void IntType::accept(Visitor *v) { v->visit(this); }

/* Prints the current AST node.
 *
 * PARAMETERS:
 *   os      - the output stream
 */
void IntType::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    os << ")";
    decIndent(os);
}

ArrayType::ArrayType(Type *elem_type, DimList* dims, Location *l) : elem_type(elem_type), dims(dims) {
    setBasicInfo(ARRAY_TYPE, l);
}


void ArrayType::accept(Visitor *v) { v->visit(this); }

void ArrayType::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    os << " " << elem_type << " ";
    for (auto dim : *dims) {
        os << dim << ", ";
    }
    decIndent(os);
}

