/*****************************************************
 *  Implementation of RiscvDesc.
 *
 */

#include "asm/riscv_md.hpp"
#include "3rdparty/set.hpp"
#include "asm/offset_counter.hpp"
#include "asm/riscv_frame_manager.hpp"
#include "config.hpp"
#include "options.hpp"
#include "scope/scope.hpp"
#include "symb/symbol.hpp"
#include "tac/flow_graph.hpp"
#include "tac/tac.hpp"

#include <cstring>
#include <iomanip>
#include <sstream>

using namespace mind::assembly;
using namespace mind::tac;
using namespace mind::util;
using namespace mind;

// declaration of empty string
#define EMPTY_STR std::string()
#define WORD_SIZE 4


/* Constructor of RiscvDesc.
 *
 */
RiscvDesc::RiscvDesc(void) {
    // {GLOBAL, LOCAL, PARAMETER}
    // Actually, we only use the parameter offset counter,
    // other two options are remained for extension
    int start[3] = {0, 0, 0}; 
    int dir[3] = {+1, -1, +1};
    _counter = new OffsetCounter(start, dir);

    // initializes the register vector
    // (we regard all general-purpose registers as caller-saved, which is
    // different from the Riscv specification)
    _reg[RiscvReg::ZERO] = new RiscvReg("zero", false, false, false); // zero
    _reg[RiscvReg::RA]   = new RiscvReg("ra",   false, false, false); // return address (actually it is caller-saved, 
    _reg[RiscvReg::SP]   = new RiscvReg("sp",   false, false, false); // stack pointer      but we already handle it in the frame manager)
    _reg[RiscvReg::GP]   = new RiscvReg("gp",   false, false, false); // global pointer
    _reg[RiscvReg::TP]   = new RiscvReg("tp",   false, false, false); // thread pointer
    _reg[RiscvReg::T0]   = new RiscvReg("t0",   true,  true,  false);
    _reg[RiscvReg::T1]   = new RiscvReg("t1",   true,  true,  false);
    _reg[RiscvReg::T2]   = new RiscvReg("t2",   true,  true,  false);
    _reg[RiscvReg::T3]   = new RiscvReg("t3",   true,  true,  false);
    _reg[RiscvReg::T4]   = new RiscvReg("t4",   true,  true,  false);
    _reg[RiscvReg::T5]   = new RiscvReg("t5",   true,  true,  false);
    _reg[RiscvReg::T6]   = new RiscvReg("t6",   true,  true,  false); // t0-t6 are caller-saved
    _reg[RiscvReg::FP]   = new RiscvReg("fp",   false, false, false); // frame pointer
    _reg[RiscvReg::S1]   = new RiscvReg("s1",   true,  false, true ); 
    _reg[RiscvReg::S2]   = new RiscvReg("s2",   true,  false, true );
    _reg[RiscvReg::S3]   = new RiscvReg("s3",   true,  false, true );
    _reg[RiscvReg::S4]   = new RiscvReg("s4",   true,  false, true );
    _reg[RiscvReg::S5]   = new RiscvReg("s5",   true,  false, true );
    _reg[RiscvReg::S6]   = new RiscvReg("s6",   true,  false, true );
    _reg[RiscvReg::S7]   = new RiscvReg("s7",   true,  false, true );
    _reg[RiscvReg::S8]   = new RiscvReg("s8",   true,  false, true );
    _reg[RiscvReg::S9]   = new RiscvReg("s9",   true,  false, true );
    _reg[RiscvReg::S10]  = new RiscvReg("s10",  true,  false, true );
    _reg[RiscvReg::S11]  = new RiscvReg("s11",  true,  false, true ); // s1-s11 are callee-saved
    _reg[RiscvReg::A0]   = new RiscvReg("a0",   true,  true,  false); // argument, return value
    _reg[RiscvReg::A1]   = new RiscvReg("a1",   true,  true,  false); // argument, return value
    _reg[RiscvReg::A2]   = new RiscvReg("a2",   true,  true,  false); // argument
    _reg[RiscvReg::A3]   = new RiscvReg("a3",   true,  true,  false); // argument
    _reg[RiscvReg::A4]   = new RiscvReg("a4",   true,  true,  false); // argument
    _reg[RiscvReg::A5]   = new RiscvReg("a5",   true,  true,  false); // argument
    _reg[RiscvReg::A6]   = new RiscvReg("a6",   true,  true,  false); // argument
    _reg[RiscvReg::A7]   = new RiscvReg("a7",   true,  true,  false); // argument, a0-a7 are caller-saved

    callee_saved_regs[0] = _reg[RiscvReg::S1];
    callee_saved_regs[1] = _reg[RiscvReg::S2];
    callee_saved_regs[2] = _reg[RiscvReg::S3];
    callee_saved_regs[3] = _reg[RiscvReg::S4];
    callee_saved_regs[4] = _reg[RiscvReg::S5];
    callee_saved_regs[5] = _reg[RiscvReg::S6];
    callee_saved_regs[6] = _reg[RiscvReg::S7];
    callee_saved_regs[7] = _reg[RiscvReg::S8];
    callee_saved_regs[8] = _reg[RiscvReg::S9];
    callee_saved_regs[9] = _reg[RiscvReg::S10];
    callee_saved_regs[10] = _reg[RiscvReg::S11];



    _lastUsedReg = 0;
    _label_counter = 0;
}

/* Gets the offset counter for this machine.
 *
 * RETURNS:
 *   the offset counter for Riscv
 */
OffsetCounter *RiscvDesc::getOffsetCounter(void) { return _counter; }

/* Translates the given Piece list into assembly code and output.
 *
 * PARAMETERS:
 *   ps    - the Piece list
 *   os    - the output stream
 */
void RiscvDesc::emitPieces(scope::GlobalScope *gscope, Piece *ps,
                           std::ostream &os) {

    _result = &os;
    // output to .data and .bss segment
    // std::ostringstream _data, _bss;

    util::List<symb::Variable*> bss_vars;
    util::List<symb::Variable*> data_vars;
    util::List<symb::Function*> weak_funcs;

    if (Option::getLevel() == Option::ASMGEN) {
        // program preamble
        for (auto it = gscope->begin(); it != gscope->end(); ++it) {
            symb::Variable *var = dynamic_cast<symb::Variable*>(*it);
            symb::Function *func = dynamic_cast<symb::Function*>(*it);
            if (var) {
                if (var->isGlobalVar()) {
                    if(var->getGlobalInit() == 0){
                        bss_vars.append(var);
                    } else {
                        data_vars.append(var);
                    }
                }
            } else if (func) {
                if (func->weak) {
                    weak_funcs.append(func);
                }
            }


        }

        // .data segment
        emit(EMPTY_STR, ".data", NULL);
        emit(EMPTY_STR, ".align 4", NULL);


        for (auto it = data_vars.begin(); it != data_vars.end(); ++it) {
            symb::Variable *sym = *it;
            emit(EMPTY_STR, (".global " + sym->getName()).c_str(), NULL);
            emit(sym->getName(), NULL, NULL);
            emit(EMPTY_STR, (".word " + std::to_string(sym->getGlobalInit())).c_str(), NULL);
        }

        // .bss segment
        emit(EMPTY_STR, ".bss", NULL);
        emit(EMPTY_STR, ".align 4", NULL);

        for (auto it = bss_vars.begin(); it != bss_vars.end(); ++it) {
            symb::Variable *sym = *it;
            emit(EMPTY_STR, (".global " + sym->getName()).c_str(), NULL);
            emit(sym->getName(), NULL, NULL);
            emit(EMPTY_STR,(".zero " + std::to_string(sym->getType()->getSize())).c_str(), NULL);
        }



        emit(EMPTY_STR, ".text", NULL);
        for(auto it = weak_funcs.begin(); it != weak_funcs.end(); ++it){
            symb::Function *func = *it;
            emit(EMPTY_STR, (".globl " + func->getName()).c_str(), NULL);
        }
        emit(EMPTY_STR, ".globl main", NULL);
        emit(EMPTY_STR, ".align 2", NULL);
    }
    // translates node by node

    while (NULL != ps) {
        switch (ps->kind) {
        case Piece::FUNCTY:
            emitFuncty(ps->as.functy);
            break;

        default:
            mind_assert(false); // unreachable
            break;
        }

        ps = ps->next;
    }
}

/* Allocates a new label (for a basic block).
 *
 * RETURNS:
 *   a new label guaranteed to be non-conflict with the existing ones
 */
const char *RiscvDesc::getNewLabel(void) {
    mind_assert(_label_counter < 10000);

    char *buf = new char[10];
    std::sprintf(buf, "__LL%d", _label_counter++);

    return buf;
}

/* Translates a single basic block into Riscv instructions.
 *
 * PARAMETERS:
 *   b     - the basic block to translate
 *   g     - the control-flow graph
 * RETURNS:
 *   the Riscv instruction sequence of this basic block
 */
RiscvInstr *RiscvDesc::prepareSingleChain(BasicBlock *b, FlowGraph *g) {
    RiscvInstr leading;
    int r0;

    _tail = &leading;
    for (Tac *t = b->tac_chain; t != NULL; t = t->next)
        emitTac(t);

    switch (b->end_kind) {
    case BasicBlock::BY_JUMP:
        spillDirtyRegs(b->LiveOut);
        addInstr(RiscvInstr::J, NULL, NULL, NULL, 0,
                 std::string(g->getBlock(b->next[0])->entry_label), {});
        // "B" for "branch"
        break;

    case BasicBlock::BY_JZERO:
        r0 = getRegForRead(b->var, 0, b->LiveOut);
        spillDirtyRegs(b->LiveOut);
        // uses "branch if equal to zero" instruction
        addInstr(RiscvInstr::BEQZ, _reg[r0], NULL, NULL, 0,
                 std::string(g->getBlock(b->next[0])->entry_label), {});
        addInstr(RiscvInstr::J, NULL, NULL, NULL, 0,
                 std::string(g->getBlock(b->next[1])->entry_label), {});
        break;

    case BasicBlock::BY_RETURN:
        r0 = getRegForRead(b->var, 0, b->LiveOut);
        spillDirtyRegs(b->LiveOut); // just to deattach all temporary variables
        addInstr(RiscvInstr::MOVE, _reg[RiscvReg::A0], _reg[r0], NULL, 0,
                 EMPTY_STR, {});
        addInstr(RiscvInstr::MOVE, _reg[RiscvReg::SP], _reg[RiscvReg::FP], NULL,
                 0, EMPTY_STR, {});
        addInstr(RiscvInstr::LW, _reg[RiscvReg::RA], _reg[RiscvReg::FP], NULL,
                 -4, EMPTY_STR, {});
        addInstr(RiscvInstr::LW, _reg[RiscvReg::FP], _reg[RiscvReg::FP], NULL,
                 -8, EMPTY_STR, {});
        addInstr(RiscvInstr::RET, NULL, NULL, NULL, 0, EMPTY_STR, {});
        _callee_saved_counter = 11;
        break;

    default:
        mind_assert(false); // unreachable
    }
    _tail = NULL;
    return leading.next;
}

/* Translates a single TAC into Riscv instructions (and records the result.
 *
 * PARAMETERS:
 *   t     - the TAC to translate
 * SIDE-EFFECT:
 *   modifies the "_tail" field
 */
void RiscvDesc::emitTac(Tac *t) {
    std::ostringstream oss;
    t->dump(oss);
    addInstr(RiscvInstr::COMMENT, NULL, NULL, NULL, 0, EMPTY_STR, oss.str());

    switch (t->op_code) {
    case Tac::LOAD_IMM4:
        emitLoadImm4Tac(t);
        break;
    case Tac::LOAD:
        emitLoadTac(t);
        break;
    case Tac::STORE:
        emitStoreTac(t);
        break;
    case Tac::LOAD_SYMBOL:
        emitLoadSymbolTac(t);
        break;
    case Tac::ALLOC:
        emitAllocTac(t);
        break;
    case Tac::PUSH:
        emitPushTac(t);
        break;
    case Tac::POP:
        emitPopTac(t);
        break;
    case Tac::CALL:
        emitCallTac(t);
        break;
    case Tac::ASSIGN:
        emitAssignTac(t);
        break;
    case Tac::NEG:
        emitUnaryTac(RiscvInstr::NEG, t);
        break;
    case Tac::LNOT:
        emitUnaryTac(RiscvInstr::SEQZ, t);
        break;
    case Tac::BNOT:
        emitUnaryTac(RiscvInstr::NOT, t);
        break;
    
    case Tac::ADD:
        emitBinaryTac(RiscvInstr::ADD, t);
        break;
    case Tac::SUB:
        emitBinaryTac(RiscvInstr::SUB, t);
        break;
    case Tac::MUL:
        emitBinaryTac(RiscvInstr::MUL, t);
        break;
    case Tac::DIV:
        emitBinaryTac(RiscvInstr::DIV, t);
        break;
    case Tac::MOD:
        emitBinaryTac(RiscvInstr::REM, t);
        break;
    
    case Tac::EQU:
        emitBinaryTac(RiscvInstr::_SEQ, t);
        break;
    case Tac::NEQ:
        emitBinaryTac(RiscvInstr::_SNE, t);
        break;
    case Tac::GRT:
        emitBinaryTac(RiscvInstr::SGT, t);
        break;
    case Tac::LES:
        emitBinaryTac(RiscvInstr::SLT, t);
        break;
    case Tac::GEQ:
        emitBinaryTac(RiscvInstr::_SGE, t);
        break;
    case Tac::LEQ:
        emitBinaryTac(RiscvInstr::_SLE, t);
        break;
    case Tac::LAND:
        emitBinaryTac(RiscvInstr::_SLAND, t);
        break;
    case Tac::LOR:
        emitBinaryTac(RiscvInstr::_SLOR, t);
        break;
    
    case Tac::CALLEE_SAVE:
        emitCalleeSaveTac(t);
        break;
    case Tac::CALLEE_RESTORE:
        emitCalleeRestoreTac(t);
        break;
    

    default:
        mind_assert(false); // should not appear inside a basic block
    }
}

void RiscvDesc::emitCalleeSaveTac(Tac *t) {
    
    callee_saved_regs[_callee_saved_counter]->var = t->op0.var;
    callee_saved_regs[_callee_saved_counter]->dirty = true;
    _callee_saved_counter++;
    // std::cerr << "save $s" << _callee_saved_counter << " in " << t->op0.var << std::endl;
}

void RiscvDesc::emitCalleeRestoreTac(Tac *t) {
    _callee_saved_counter--;
    if(callee_saved_regs[_callee_saved_counter]->var != t->op0.var) {
        // op0 is on the stack
        mind_assert(t->op0.var->is_offset_fixed);
        addInstr(RiscvInstr::LW, callee_saved_regs[_callee_saved_counter], _reg[RiscvReg::FP], NULL, t->op0.var->offset, EMPTY_STR, {});
    }
}



/* Translates a LoadImm4 TAC into Riscv instructions.
 *
 * PARAMETERS:
 *   t     - the LoadImm4 TAC
 */
void RiscvDesc::emitLoadImm4Tac(Tac *t) {
    // eliminates useless assignments
    if (!t->LiveOut->contains(t->op0.var)) {
        addInstr(RiscvInstr::COMMENT, NULL, NULL, NULL, 0, EMPTY_STR, "useless code");
        return;
    }
        

    // uses "load immediate number" instruction
    int r0 = getRegForWrite(t->op0.var, 0, 0, t->LiveOut);
    addInstr(RiscvInstr::LI, _reg[r0], NULL, NULL, t->op1.ival, EMPTY_STR,
             {});
}

void RiscvDesc::emitLoadSymbolTac(Tac *t) {
    // eliminates useless assignments
    if (!t->LiveOut->contains(t->op0.var)) {
        addInstr(RiscvInstr::COMMENT, NULL, NULL, NULL, 0, EMPTY_STR, "useless code: T" + std::to_string(t->op0.var->id) + " is not used");
        return;
    }
        
    int r0 = getRegForWrite(t->op0.var, 0, 0, t->LiveOut);
    addInstr(RiscvInstr::LA, _reg[r0], NULL, NULL, 0, t->op1.name, {});
}

/* Translates a Load TAC into Riscv instructions.
 *
 * PARAMETERS:
 *   t     - the Load TAC
 */

void RiscvDesc::emitLoadTac(Tac *t) {
    // eliminates useless assignments
    if (!t->LiveOut->contains(t->op0.var)) {
        addInstr(RiscvInstr::COMMENT, NULL, NULL, NULL, 0, EMPTY_STR, "useless code");
        return;
    }
        
    // uses "load word" instruction
    int r1 = getRegForRead(t->op1.var, 0, t->LiveOut);
    int r0 = getRegForWrite(t->op0.var, r1, 0, t->LiveOut);
    
    addInstr(RiscvInstr::LW, _reg[r0], _reg[r1], NULL, t->op2.ival, EMPTY_STR,
             {});

}

/* Translates a Store TAC into Riscv instructions.
 *
 * PARAMETERS:
 *   t     - the Store TAC
 */
void RiscvDesc::emitStoreTac(Tac *t) {
    // uses "store word" instruction
    int r0 = getRegForRead(t->op0.var, 0, t->LiveOut);
    int r1 = getRegForRead(t->op1.var, r0, t->LiveOut);
    addInstr(RiscvInstr::SW, _reg[r0], _reg[r1], NULL, t->op2.ival, EMPTY_STR,
             {});
}

/* Translates an Alloc TAC into Riscv instructions.
 *
 * PARAMETERS:
 *   t     - the Alloc TAC
 */
void RiscvDesc::emitAllocTac(Tac *t) {
    // eliminates useless assignments
    if (!t->LiveOut->contains(t->op0.var)) {
        addInstr(RiscvInstr::COMMENT, NULL, NULL, NULL, 0, EMPTY_STR, "useless code");
        return;
    }
        
    int r0 = getRegForWrite(t->op0.var, 0, 0, t->LiveOut);

    std::ostringstream cmt;
    cmt << "get address ( fp " << t->op2.ival << " ) size: " << t->op1.ival;
    addInstr(RiscvInstr::ADDI, _reg[r0], _reg[RiscvReg::FP], NULL, t->op2.ival, {}, cmt.str());
}

void RiscvDesc::emitPushTac(Tac *t) {

    if (_param_counter < 8) {
        passParamReg(t, _param_counter); // pass param in register
    } else {
        if (_param_counter == 8) {
            // save caller saved registers, because this function may
            // modify sp, we must call it before storing params on stack
            spillCallerSavedDirtyRegs(t->LiveOut);
        }
        
        int i = lookupReg(t->op0.var);
        int r0;
        if (i<0){ // on the stack
            // always use t0 to transfer param, we already spill caller saved regs
            r0 = RiscvReg::T0;
            addInstr(RiscvInstr::LW, _reg[RiscvReg::T0], _reg[RiscvReg::FP], NULL, t->op0.var->offset, EMPTY_STR, {});
        } else{
            r0 = i;
        }
        addInstr(RiscvInstr::SW, _reg[r0], _reg[RiscvReg::SP], NULL, (-4*(_param_counter-7)), EMPTY_STR,
            {});
    }

    _param_counter++;
}

void RiscvDesc::emitPopTac(Tac *t) {
    
    if (_callee_param_counter < 8) {
        // std::cerr<< "pop to "<< t->op0.var <<std::endl;
        getParamReg(t, _callee_param_counter); // get param in register
    } else {
        int r0 = getRegForWrite(t->op0.var, 0, 0, t->LiveOut);
        addInstr(RiscvInstr::LW, _reg[r0], _reg[RiscvReg::FP], NULL, 4*(_callee_param_counter-8), EMPTY_STR,
             {});
    }

    _callee_param_counter++;
}

void RiscvDesc::emitCallTac(Tac *t) {
    // std::cerr << "call " << t->op1.label->str_form << " with " << _param_counter << " params" << std::endl;
    if (_param_counter <= 8){
        spillCallerSavedDirtyRegs(t->LiveOut);
    }
    // modify sp
    if (_param_counter > 8) {
        addInstr(RiscvInstr::ADDI, _reg[RiscvReg::SP], _reg[RiscvReg::SP], NULL, -4*(_param_counter-8), EMPTY_STR,
             {});
    }


    addInstr(RiscvInstr::JAL, _reg[RiscvReg::RA], NULL, NULL, 0, t->op1.label->str_form, {});
    // restore sp
    if (_param_counter > 8) {
        addInstr(RiscvInstr::ADDI, _reg[RiscvReg::SP], _reg[RiscvReg::SP], NULL, 4*(_param_counter-8), EMPTY_STR,
             {});
    }

    // set return value
    _reg[RiscvReg::A0]->var = t->op0.var;
    _reg[RiscvReg::A0]->dirty = true;
    _param_counter = 0;
}




/* Translates an Assign TAC into Riscv instructions.
 *
 * PARAMETERS:
 *   t     - the Assign TAC
 */
void RiscvDesc::emitAssignTac(Tac *t) {
    // eliminates useless assignments
    if (!t->LiveOut->contains(t->op0.var)) {
        addInstr(RiscvInstr::COMMENT, NULL, NULL, NULL, 0, EMPTY_STR, "useless code");
        return;
    }
        

    int r1 = getRegForRead(t->op1.var, 0, t->LiveOut);
    int r0 = getRegForWrite(t->op0.var, r1, 0, t->LiveOut);
    
    addInstr(RiscvInstr::MOVE, _reg[r0], _reg[r1], NULL, 0, EMPTY_STR, {});

}

/* Translates a Unary TAC into Riscv instructions.
 *
 * PARAMETERS:
 *   t     - the Unary TAC
 */
void RiscvDesc::emitUnaryTac(RiscvInstr::OpCode op, Tac *t) {
    // eliminates useless assignments
    if (!t->LiveOut->contains(t->op0.var)) {
        addInstr(RiscvInstr::COMMENT, NULL, NULL, NULL, 0, EMPTY_STR, "useless code");
        return;
    }
            

    int r1 = getRegForRead(t->op1.var, 0, t->LiveOut);
    int r0 = getRegForWrite(t->op0.var, r1, 0, t->LiveOut);

    addInstr(op, _reg[r0], _reg[r1], NULL, 0, EMPTY_STR, {});
}

/* Translates a Binary TAC into Riscv instructions.
 *
 * PARAMETERS:
 *   t     - the Binary TAC
 */
void RiscvDesc::emitBinaryTac(RiscvInstr::OpCode op, Tac *t) {
    // eliminates useless assignments
    if (!t->LiveOut->contains(t->op0.var)){
        addInstr(RiscvInstr::COMMENT, NULL, NULL, NULL, 0, EMPTY_STR, "useless code");
        return;
    }
        

    Set<Temp>* liveness = t->LiveOut->clone();
    liveness->add(t->op1.var);
    liveness->add(t->op2.var);
    int r1 = getRegForRead(t->op1.var, 0, liveness);
    int r2 = getRegForRead(t->op2.var, r1, liveness);
    int r0 = getRegForWrite(t->op0.var, r1, r2, liveness);

    if (op < RiscvInstr::__PSEUDO_INSTRUCTION){
        addInstr(op, _reg[r0], _reg[r1], _reg[r2], 0, EMPTY_STR, {});
    }else{
        switch(op){
        case RiscvInstr::_SEQ:
            addInstr(RiscvInstr::SUB, _reg[r0], _reg[r1], _reg[r2], 0, EMPTY_STR, {});
            addInstr(RiscvInstr::SEQZ, _reg[r0], _reg[r0], NULL, 0, EMPTY_STR, {});
            break;
        case RiscvInstr::_SNE:
            addInstr(RiscvInstr::SUB, _reg[r0], _reg[r1], _reg[r2], 0, EMPTY_STR, {});
            addInstr(RiscvInstr::SNEZ, _reg[r0], _reg[r0], NULL, 0, EMPTY_STR, {});
            break;
        case RiscvInstr::_SGE:
            addInstr(RiscvInstr::SLT, _reg[r0], _reg[r1], _reg[r2], 0, EMPTY_STR, {});
            addInstr(RiscvInstr::XORI, _reg[r0], _reg[r0], NULL, 1, EMPTY_STR, {});
            break;
        case RiscvInstr::_SLE:
            addInstr(RiscvInstr::SGT, _reg[r0], _reg[r1], _reg[r2], 0, EMPTY_STR, {});
            addInstr(RiscvInstr::XORI, _reg[r0], _reg[r0], NULL, 1, EMPTY_STR, {});
            break;
        case RiscvInstr::_SLOR:
            addInstr(RiscvInstr::OR, _reg[r0], _reg[r1], _reg[r2], 0, EMPTY_STR, {});
            addInstr(RiscvInstr::SNEZ, _reg[r0], _reg[r0], NULL, 0, EMPTY_STR, {});
            break;
        case RiscvInstr::_SLAND:
            addInstr(RiscvInstr::SNEZ, _reg[r0], _reg[r1], NULL, 0, EMPTY_STR, {});
            addInstr(RiscvInstr::SUB, _reg[r0], _reg[RiscvReg::ZERO], _reg[r0], 0, EMPTY_STR, {});
            addInstr(RiscvInstr::AND, _reg[r0], _reg[r0], _reg[r2], 0, EMPTY_STR, {});
            addInstr(RiscvInstr::SNEZ, _reg[r0], _reg[r0], NULL, 0, EMPTY_STR, {});
            break;
        default:
            std::cerr << "not binary pseudo instruction: " << op << std::endl;
            mind_assert(false);
        }
    }
    
}

/* Outputs a single instruction line.
 *
 * PARAMETERS:
 *   label   - label of this line
 *   body    - instruction
 *   comment - comment of this line
 */
void RiscvDesc::emit(std::string label, const char *body, const char *comment) {
    std::ostream &os(*_result);

    if ((NULL != comment) && (label.empty()) && (NULL == body)) {
        os << "                                  # " << comment;

    } else {
        if (!label.empty())
            os << label << std::left << std::setw(40 - label.length()) << ":";
        else if (NULL != body)
            os << "          " << std::left << std::setw(30) << body;

        if (NULL != comment)
            os << "# " << comment;
    }

    os << std::endl;
}

/* Use to put a specified reg to another to pass params.
 *
 * PARAMETERS:
 *   t     - a special tac for param
 *   cnt   - reg offset A0 + cnt
 */
void RiscvDesc::passParamReg(Tac *t, int cnt) {
    t->LiveOut->add(t->op0.var);
    std::ostringstream oss;
    // RISC-V use a0-a7 to pass the first 8 parameters, so it's ok to do so.
    spillReg(RiscvReg::A0 + cnt, t->LiveOut);
    int i = lookupReg(t->op0.var);
    if(i < 0) {
        auto v = t->op0.var;
        RiscvReg *base = _reg[RiscvReg::FP];
        oss << "load " << v << " from (" << base->name
            << (v->offset < 0 ? "" : "+") << v->offset << ") into "
            << _reg[RiscvReg::A0 + cnt]->name;
        addInstr(RiscvInstr::LW, _reg[RiscvReg::A0 + cnt], base, NULL, v->offset, EMPTY_STR,
                    oss.str());
    } else {
        oss << "copy " << _reg[i]->name << " to " << _reg[RiscvReg::A0 + cnt]->name;
        addInstr(RiscvInstr::MOVE, _reg[RiscvReg::A0 + cnt], _reg[i], NULL, 0,
                    EMPTY_STR, oss.str());
    }
}

/* Use to set a param reg.
 *
 * PARAMETERS:
 *   t     - a special tac for param
 *   cnt   - reg offset A0 + cnt
 */
void RiscvDesc::getParamReg(Tac *t, int cnt) {
    _reg[RiscvReg::A0 + cnt]->var = t->op0.var;
    _reg[RiscvReg::A0 + cnt]->dirty = true;
}

/* Translates a "Functy" object into assembly code and output.
 *
 * PARAMETERS:
 *   f     - the Functy object
 */
void RiscvDesc::emitFuncty(Functy f) {
    mind_assert(NULL != f);

    _param_counter = 0;
    _callee_param_counter = 0;
    _callee_saved_counter = 0;

    
    FlowGraph *g = FlowGraph::makeGraph(f);
    g->simplify();        // simple optimization
    g->analyzeLiveness(); // computes LiveOut set of the basic blocks

    
    
    _frame = new RiscvStackFrameManager(-3 * WORD_SIZE);

    // find all Alloc
    for (FlowGraph::iterator it = g->begin(); it != g->end(); ++it) {
        BasicBlock *b = *it;
        
        tac::Tac* tacs = b->tac_chain;
        while (tacs != NULL) {
            if (tacs->op_code == tac::Tac::ALLOC) {
                tacs->op2.ival = _frame->alloc(tacs->op1.ival);
            }
            tacs = tacs->next;
        }
        
    }

    for (FlowGraph::iterator it = g->begin(); it != g->end(); ++it) {
        // all variables shared between basic blocks should be reserved
        Set<Temp> *liveout = (*it)->LiveOut;
        for (Set<Temp>::iterator sit = liveout->begin(); sit != liveout->end();
             ++sit) {
            _frame->reserve(*sit);
        }
        (*it)->entry_label = getNewLabel(); // adds entry label of a basic block
    }
    for (FlowGraph::iterator it = g->begin(); it != g->end(); ++it) {
        BasicBlock *b = *it;
        b->analyzeLiveness(); // computes LiveOut set of every TAC
        _frame->reset();
        // translates the TAC sequences of this block
        b->instr_chain = prepareSingleChain(b, g);
        if (Option::doOptimize()) // use "-O" option to enable optimization
            simplePeephole((RiscvInstr *)b->instr_chain);
        b->mark = 0; // clears the marks (for the next step)
    }
    if (Option::getLevel() == Option::DATAFLOW) {
        std::cout << "Control-flow Graph of " << f->entry << ":" << std::endl;
        g->dump(std::cout);
        // TO STUDENTS: You might not want to get lots of outputs when
        // debugging.
        //              You can enable the following line so that the program
        //              will terminate after the first Functy is done.
        // std::exit(0);
        return;
    }

    mind_assert(!f->entry->str_form.empty()); // this assertion should hold for every Functy
    

    std::ostream* old_stream = _result;
    std::ostringstream oss;
    _result = &oss; // emit trace to oss first (we need to get actual stack size)

    // chains up the assembly code of every basic block and output.
    //
    // ``A trace is a sequence of statements that could be consecutively
    //   executed during the execution of the program. It can include
    //   conditional branches.''
    //           -- Modern Compiler Implementation in Java (the ``Tiger Book'')
    for (FlowGraph::iterator it = g->begin(); it != g->end(); ++it)
        emitTrace(*it, g);

    _result = old_stream;

    // outputs the header of a function
    emitProlog(f->entry, _frame->getStackFrameSize());

    // outputs the assembly code of the function
    *_result << oss.str();



}

/* Outputs the leading code of a function.
 *
 * PARAMETERS:
 *   entry_label - the function label
 *   frame_size  - stack-frame size of this function
 * NOTE:
 *   the prolog code is used to save context and establish the stack frame.
 */
void RiscvDesc::emitProlog(Label entry_label, int frame_size) {
    std::ostringstream oss;

    emit(EMPTY_STR, NULL, NULL); // an empty line
    emit(EMPTY_STR, ".text", NULL);
    if (entry_label->str_form == "main") {
        oss << "main";
    } else {
        oss << entry_label;
    }
    emit(oss.str(), NULL, "function entry"); // marks the function entry label
    oss.str("");
    // saves old context
    emit(EMPTY_STR, "sw    ra, -4(sp)", NULL); // saves return address
    emit(EMPTY_STR, "sw    fp, -8(sp)", NULL); // saves old frame pointer
    // establishes new stack frame (new context)
    emit(EMPTY_STR, "mv    fp, sp", NULL);
    oss << "addi  sp, sp, -" << (frame_size + 2 * WORD_SIZE); // 2 WORD's for old $fp and $ra
    emit(EMPTY_STR, oss.str().c_str(), NULL);
}

/* Outputs a single instruction.
 *
 * PARAMETERS:
 *   i     - the instruction to output
 */
void RiscvDesc::emitInstr(RiscvInstr *i) {
    if (i->cancelled)
        return;
    
    mind_assert(i->op_code < RiscvInstr::__PSEUDO_INSTRUCTION);

    std::ostringstream oss;
    oss << std::left << std::setw(6);

    switch (i->op_code) {
    case RiscvInstr::COMMENT:
        emit(EMPTY_STR, NULL, i->comment.c_str());
        return;

    case RiscvInstr::LI:
        oss << "li" << i->r0->name << ", " << i->i;
        break;
    
    case RiscvInstr::LA:
        oss << "la" << i->r0->name << ", " << i->l;
        break;

    case RiscvInstr::NEG:
        oss << "neg" << i->r0->name << ", " << i->r1->name;
        break;

    case RiscvInstr::SEQZ:
        oss << "seqz" << i->r0->name << ", " << i->r1->name;
        break;
    
    case RiscvInstr::SNEZ:
        oss << "snez" << i->r0->name << ", " << i->r1->name;
        break;

    case RiscvInstr::NOT:
        oss << "not" << i->r0->name << ", " << i->r1->name;
        break;

    case RiscvInstr::MOVE:
        oss << "mv" << i->r0->name << ", " << i->r1->name;
        break;

    case RiscvInstr::LW:
        oss << "lw" << i->r0->name << ", " << i->i << "(" << i->r1->name << ")";
        break;

    case RiscvInstr::SW:
        oss << "sw" << i->r0->name << ", " << i->i << "(" << i->r1->name << ")";
        break;
    
    case RiscvInstr::JAL:
        oss << "jal" << i->r0->name << ", " << i->l;
        break;

    case RiscvInstr::RET:
        oss << "ret";
        break;
    
    case RiscvInstr::ADD:
        oss << "add" << i->r0->name << ", " << i->r1->name << ", " << i->r2->name;
        break;

    case RiscvInstr::SUB:
        oss << "sub" << i->r0->name << ", " << i->r1->name << ", " << i->r2->name;
        break;

    case RiscvInstr::MUL:
        oss << "mul" << i->r0->name << ", " << i->r1->name << ", " << i->r2->name;
        break;

    case RiscvInstr::DIV:
        oss << "div" << i->r0->name << ", " << i->r1->name << ", " << i->r2->name;
        break;

    case RiscvInstr::REM:
        oss << "rem" << i->r0->name << ", " << i->r1->name << ", " << i->r2->name;
        break;

    case RiscvInstr::SLT:
        oss << "slt" << i->r0->name << ", " << i->r1->name << ", " << i->r2->name;
        break;

    case RiscvInstr::SGT:
        oss << "sgt" << i->r0->name << ", " << i->r1->name << ", " << i->r2->name;
        break;

    case RiscvInstr::AND:
        oss << "and" << i->r0->name << ", " << i->r1->name << ", " << i->r2->name;
        break;

    case RiscvInstr::OR:
        oss << "or" << i->r0->name << ", " << i->r1->name << ", " << i->r2->name;
        break;

    case RiscvInstr::XOR:
        oss << "xor" << i->r0->name << ", " << i->r1->name << ", " << i->r2->name;
        break;

    case RiscvInstr::ADDI:
        oss << "addi" << i->r0->name << ", " << i->r1->name << ", " << i->i;
        break;

    case RiscvInstr::XORI:
        oss << "xori" << i->r0->name << ", " << i->r1->name << ", " << i->i;
        break;
    
    case RiscvInstr::BEQZ:
        oss << "beqz" << i->r0->name << ", " << i->l;
        break;

    case RiscvInstr::J:
        oss << "j" << i->l;
        break;


    default:
        mind_assert(false); // other instructions not supported
    }

    emit(EMPTY_STR, oss.str().c_str(), (i->comment.size()) ? i->comment.c_str() : NULL);
}

/* Outputs a "trace" (see also: RiscvDesc::emitFuncty).
 *
 * PARAMETERS:
 *   b     - the leading basic block of this trace
 *   g     - the control-flow graph
 * NOTE:
 *   we just do a simple depth-first search against the CFG
 */
void RiscvDesc::emitTrace(BasicBlock *b, FlowGraph *g) {
    // a trace is a series of consecutive basic blocks
    if (b->mark > 0)
        return;
    b->mark = 1;
    emit(std::string(b->entry_label), NULL, NULL);

    RiscvInstr *i = (RiscvInstr *)b->instr_chain;
    while (NULL != i) {
        emitInstr(i);
        i = i->next;
    }
    switch (b->end_kind) {
    case BasicBlock::BY_JUMP:
        emitTrace(g->getBlock(b->next[0]), g);
        break;

    case BasicBlock::BY_JZERO:
        emitTrace(g->getBlock(b->next[1]), g);
        break;

    case BasicBlock::BY_RETURN:
        break;

    default:
        mind_assert(false); // unreachable
    }
}

/* Appends an instruction line to "_tail". (internal helper function)
 *
 * PARAMETERS:
 *   op_code - operation code
 *   r0      - the first register operand (if any)
 *   r1      - the second register operand (if any)
 *   r2      - the third register operand (if any)
 *   i       - immediate number or offset (if any)
 *   l       - label operand (for LA and jumps)
 *   cmt     - comment of this line
 */
void RiscvDesc::addInstr(RiscvInstr::OpCode op_code, RiscvReg *r0, RiscvReg *r1,
                         RiscvReg *r2, int i, std::string l, const std::string& cmt) {
    mind_assert(NULL != _tail);

    // we should eliminate all the comments when doing optimization
    if (Option::doOptimize() && (RiscvInstr::COMMENT == op_code))
        return;
    _tail->next = new RiscvInstr();
    _tail = _tail->next;
    _tail->op_code = op_code;
    _tail->r0 = r0;
    _tail->r1 = r1;
    _tail->r2 = r2;
    _tail->i = i;
    _tail->l = l;
    _tail->comment = cmt;
}


/******************** a simple peephole optimizer *********************/

/* Performs a peephole optimization pass to the instruction sequence.
 *
 * PARAMETERS:
 *   iseq  - the instruction sequence to optimize
 */
void RiscvDesc::simplePeephole(RiscvInstr *iseq) {
    // if you are interested in peephole optimization, you can implement here
    // of course, beyond our requirements
    
}

/******************* REGISTER ALLOCATOR ***********************/

/* Acquires a register to read some variable.
 *
 * PARAMETERS:
 *   v      - the variable to read
 *   avoid1 - the register which should not be selected
 *   live   - current liveness set
 * RETURNS:
 *   number of the register containing the content of v
 */
int RiscvDesc::getRegForRead(Temp v, int avoid1, LiveSet *live) {
    std::ostringstream oss;

    int i = lookupReg(v);

    if (i < 0) {
        // we will load the content into some register

        // find empty register
        i = lookupReg(NULL);

        if (i < 0) {
            // no empty register, we need to spill some register
            i = selectRegToSpill(avoid1, RiscvReg::ZERO, live);
            spillReg(i, live);
        }

        _reg[i]->var = v;

       
        if (v->is_offset_fixed) {
             // has been allocated on the stack, load it
            RiscvReg *base = _reg[RiscvReg::FP];
            oss << "load " << v << " from (" << base->name
                << (v->offset < 0 ? "" : "+") << v->offset << ") into "
                << _reg[i]->name;
            addInstr(RiscvInstr::LW, _reg[i], base, NULL, v->offset, EMPTY_STR,
                     oss.str());

        } else {
            oss << "initialize " << v << " with 0";
            addInstr(RiscvInstr::MOVE, _reg[i], _reg[RiscvReg::ZERO], NULL, 0,
                     EMPTY_STR, oss.str());
        }
        _reg[i]->dirty = false;
    }

    return i;
}

/* Acquires a register to write some variable.
 *
 * PARAMETERS:
 *   v      - the variable to write
 *   avoid1 - the register which should not be selected
 *   avoid2 - the same as "avoid1"
 *   live   - the current liveness set
 * RETURNS:
 *   number of the register which can be safely written to
 */
int RiscvDesc::getRegForWrite(Temp v, int avoid1, int avoid2, LiveSet *live) {
    if (NULL == v || !live->contains(v))
        return RiscvReg::ZERO;

    int i = lookupReg(v);

    if (i < 0) {
        i = lookupReg(NULL);

        if (i < 0) {
            i = selectRegToSpill(avoid1, avoid2, live);
            spillReg(i, live);
        }
        
        _reg[i]->var = v;
    }

    _reg[i]->dirty = true;

    
    return i;
}

/* Spills a specified register (into memory, i.e. into the stack-frame).
 *
 * PARAMETERS:
 *   i     - number of the register to spill
 *   live  - the current liveness set
 * NOTE:
 *   if the variable contained in $i is no longer alive,
 *   we don't save it into memory.
 */
void RiscvDesc::spillReg(int i, LiveSet *live) {
    std::ostringstream oss;

    Temp v = _reg[i]->var;

    if ((NULL != v) && _reg[i]->dirty && live->contains(v)) {
        RiscvReg *base = _reg[RiscvReg::FP];

        if (!v->is_offset_fixed) {
            // allocate space for this variable on the stack
            _frame->getSlotToWrite(v, live);
        }

        oss << "spill " << v << " from " << _reg[i]->name << " to ("
            << base->name << (v->offset < 0 ? "" : "+") << v->offset << ")";
        addInstr(RiscvInstr::SW, _reg[i], base, NULL, v->offset, EMPTY_STR,
                 oss.str());
    }

    _reg[i]->var = NULL;
    _reg[i]->dirty = false;
}

/* Spills all dirty (and alive) registers into memory.
 *
 * PARAMETERS:
 *   live  - the current liveness set
 */
void RiscvDesc::spillDirtyRegs(LiveSet *live) {
    int i;
    // determines whether we should spill the registers
    for (i = 0; i < RiscvReg::TOTAL_NUM; ++i) {
        if ((NULL != _reg[i]->var) && _reg[i]->dirty &&
            live->contains(_reg[i]->var))
            break;

        _reg[i]->var = NULL;
        _reg[i]->dirty = false;
    }

    if (i < RiscvReg::TOTAL_NUM) {
        addInstr(RiscvInstr::COMMENT, NULL, NULL, NULL, 0, EMPTY_STR,
                 "(save modified registers before control flow changes)");

        for (; i < RiscvReg::TOTAL_NUM; ++i)
            spillReg(i, live);
    }
}



void RiscvDesc::spillCallerSavedDirtyRegs(LiveSet *live) {
    
    addInstr(RiscvInstr::COMMENT, NULL, NULL, NULL, 0, EMPTY_STR,
                 "(save caller saved registers)");

    for (int i = 0; i < RiscvReg::TOTAL_NUM; ++i) {
        if(_reg[i]->caller_saved) {
            spillReg(i, live);
        }
    }

}

/* Looks up a register containing the specified variable.
 *
 * PARAMETERS:
 *   v     - the specified variable
 * RETURNS:
 *   number of the register if found; -1 if not found
 */
int RiscvDesc::lookupReg(tac::Temp v) {
    for (int i = 0; i < RiscvReg::TOTAL_NUM; ++i)
        if (_reg[i]->general && _reg[i]->var == v)
            return i;

    return -1;
}

/* Selects a register to spill into memory (so that it can be released).
 *
 * PARAMETERS:
 *   avoid1 - the register that should not be selected
 *   avoid2 - the same as avoid1
 *   live   - the current liveness set
 * RETURNS:
 *   number of the selected register
 */
int RiscvDesc::selectRegToSpill(int avoid1, int avoid2, LiveSet *live) {
    // looks for a "ready" one
    for (int i = 0; i < RiscvReg::TOTAL_NUM; ++i) {
        if (!_reg[i]->general)
            continue;

        if ((i != avoid1) && (i != avoid2) && !live->contains(_reg[i]->var))
            return i;
    }

    // looks for a clean one (so that we could save a "store")
    for (int i = 0; i < RiscvReg::TOTAL_NUM; ++i) {
        if (!_reg[i]->general)
            continue;

        if ((i != avoid1) && (i != avoid2) && !_reg[i]->dirty)
            return i;
    }

    // the worst case: all are live and all are dirty.
    // chooses one register w.r.t a policy similar to the LRU algorithm (random
    // choice)
    do {
        _lastUsedReg = (_lastUsedReg + 1) % RiscvReg::TOTAL_NUM;
    } while ((_lastUsedReg == avoid1) || (_lastUsedReg == avoid2) ||
             !_reg[_lastUsedReg]->general);

    return _lastUsedReg;
}
