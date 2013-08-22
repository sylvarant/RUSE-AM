/*
 * =====================================================================================
 *
 *       Filename:  cesk.c
 *
 *    Description:  Cesk machine in c
 *
 *        Created:  10/17/2012 14:54:40
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#include "cesk.h"
#include "load.h"
#include "string.h"
#include "type.h"


/*-----------------------------------------------------------------------------
 *  Preprocessor
 *-----------------------------------------------------------------------------*/
#define NELEMS(x)  (sizeof(x) / sizeof(x[0]))


/*-----------------------------------------------------------------------------
 * Sancus spm
 *-----------------------------------------------------------------------------*/
#if defined(SECURE) && defined(SANCUS_SPM)
DECLARE_SM(secure_vm,0x1234);
#endif


/*-----------------------------------------------------------------------------
 *  Local Functions
 *-----------------------------------------------------------------------------*/

LOCAL LIMBO step(void);
LOCAL LIMBO apply(VALUE proc,VALUE * args);
LOCAL LIMBO applyKont(VALUE val,N(Kont) k);
LOCAL VALUE evalAtom(VALUE atom);
LOCAL unsigned int isAtom(VALUE atom);

#ifdef SECURE
LOCAL VALUE convertin(OTHERVALUE ptr,TYPE goal);
LOCAL void * convertout(VALUE in,TYPE goal);
LOCAL void function_types(TYPE goal,TYPE * left, TYPE * right);
#endif


/*-----------------------------------------------------------------------------
 *  File Members
 *-----------------------------------------------------------------------------*/

// this is the state of the cesk machine,
// which is local to the file for security reasons
SECRET_DATA STATE * mystate = NULL;

#ifdef SECURE
LOCAL unsigned int load = 0;
LOCAL void* (*insecure_eval)(void*) = NULL;
#endif


/*-----------------------------------------------------------------------------
 *  Debug -- TODO remove or fix
 *-----------------------------------------------------------------------------*/

#ifdef DEBUG
LOCAL void debugState(void);

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    debugState
 *  Description:    print the current cesk state
 * =====================================================================================
 */
LOCAL void debugState(){

    DEBUG_PRINT("=========================="); 
    DEBUG_PRINT("** CONTROL");

    char * ctrl = N(toString)(mystate->control,0);
    DEBUG_PRINT("%s",ctrl); 
    free(ctrl);
    DEBUG_PRINT("** ENVIRONMENT : "); 
    BINDING *node = mystate->env;
    while(node){
        char * str =  N(toString)(*(node->address),0);
        DEBUG_PRINT("%s => %s ",node->key,str);
        node = node->next;
        free(str);
    }
    DEBUG_PRINT("** CONTINUATION"); 
    VALUE cc; 
	cc.b = N(makeContinuation)(mystate->cont.empty);
    char * strc = N(toString)(cc,0);
    DEBUG_PRINT(" --> cont type %s",strc);
    free(strc);

    #ifdef SECURE
    DEBUG_PRINT("** Labels"); 
    LABEL * snode = mystate->label;
    while(snode != NULL){ 
        char * strd =  N(toString)(snode->an->t,0);
        DEBUG_PRINT("--> Label == %d - %s",snode->label,strd);
        snode = snode->next;
        free(strd);
    }
    #endif
    DEBUG_PRINT("=========================="); 
}

#endif


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    isAtom
 *  Description:    predicate to identify atomic statements
 * =====================================================================================
 */
LOCAL unsigned int isAtom(VALUE el)
{
    enum N(Tag) atoms[] = {   N(LAM),N(INT),N(SYMBOL),N(BOOLEAN),N(PRIM),
                            N(LIST),N(QUOTE),N(CLOSURE),
                            #ifdef SECURE
                                SI
                            #else
                                IS
                            #endif
                        };
    if(el.tt == N(UNIT) || el.tt == N(NOP)) return 1;
    for(unsigned int i = 0; i < NELEMS(atoms); i++){ 
        if(el.b->t == atoms[i]){return 1;}
    }
    return 0;
}
    
#ifdef SECURE
/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    convertin
 *  Description:    convert the low level values into secure
 * =====================================================================================
 */
LOCAL VALUE convertin(OTHERVALUE ptr,TYPE goal){

    switch(ptr.b->t){

        case OTHERN(BOOLEAN) : {

            // check value
            if(ptr.b->value == RFALSE || ptr.b->value == RTRUE){

                // check type
                if(N(checkType)(ptr,goal)){

                    // build value
			        VALUE v;
				    v.b =  N(makeBoolean)(ptr.b->value);
                    return v;
                }
            }
            DEBUG_PRINT("Boolean conversion failed");
            exit(1);
        }

        case OTHERN(INT) : { 
            if(N(checkType)(ptr,goal)){

                // value check is simply done by taking bytes
			    VALUE v;
			    v.b = N(makeInt)(ptr.z->value);
                return v;          
			}
            DEBUG_PRINT("Int conversion failed");
            exit(1);
        }

        case OTHERN(UNIT) : {
            if(N(checkType)(ptr,goal)){
                VALUE v;
                v.b = makeUnit();
                return v;
            }
            DEBUG_PRINT("Unit conversion failed");
            exit(1);
        }

        default : {
                VALUE v;
                v.b =  makeSI(goal.byte,ptr.b);
                return v;
            }
    }
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    convertout
 *  Description:    convert the low level values into insecure
 * =====================================================================================
 */
LOCAL void * convertout(VALUE in,TYPE goal){

    switch(in.b->t){

        case N(BOOLEAN) :
            return (OTHERN(makeBoolean)(in.b->value)); 

        case N(INT) :
            return (OTHERN(makeInt)(in.z->value)); 

        // TODO is this secure ?
        default :{
                
            // label -> in -> goal
            ANNOTATION * code = MALLOC(sizeof(ANNOTATION)); 
            code->t  = in;
            code->ty = goal;
            int label = N(newLabel)();
            N(insertLabel)( &(mystate->label),label,code);
            DEBUG_PRINT("Adding Label (A) == %d",label);

            // IS : goal : label
            return makeIS(goal.byte,label);
        }
    }
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    function_types
 *  Description:    extract left and right types from the goal
 * =====================================================================================
 */
LOCAL void function_types(TYPE goal,TYPE * left, TYPE * right){

    // We have to check t1->t2
    switch(N(language)){
                    
        case SCHEME : {
            left->byte  = N(makeTIgnore)();
            right->byte = N(makeTIgnore)();
            break;
        }
                        
        case ML : {
            if(goal.a->t != N(TARROW)){
                DEBUG_PRINT("Closure requires arrow type");
                exit(1);
            }
            (*left)  = goal.a->left;
            (*right) = goal.a->right;
            break;
        }

        default :{
            DEBUG_PRINT("Language not supported");
            exit(1); 
        }
    }
}

#endif

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    evalAtom
 *  Description:    evaluate atomic expressions 
 *  Todo       :    implement primitives
 * =====================================================================================
 */
FUNCTIONALITY VALUE evalAtom(VALUE atom){

    switch(atom.tt){
        
        case N(NOP):
        case N(UNIT) :
            return atom;

        default : break;
    }

    switch(atom.b->t){

    case N(SYMBOL) : {
        VALUE * res = N(getBinding)(mystate->env,atom.s->name); 

        if(res == NULL){ 
            DEBUG_PRINT("Storage failure for %s",atom.s->name);
            exit(1);
        }

        if((*res).tt == N(ERROR)) {
            DEBUG_PRINT("Unintialized Binding to %s",atom.s->name);
            exit(1);
        }
        return (*res);
    }

    // for a lot of cases don't do anything
    case N(BOOLEAN) :
    case N(LIST):
    case N(INT) : 
    case N(CLOSURE) :
        return atom;
        break;


    case N(PRIM) :{
        VALUE * parsed = MALLOC(atom.p->nargs * sizeof(VALUE));
        for(int i = 0; i < atom.p->nargs; i++){
            DEBUG_PRINT(N(toString)(atom.p->arguments[i],0));
            parsed[i] = evalAtom(atom.p->arguments[i]);
            DEBUG_PRINT(N(toString)(parsed[i],0));
        }
        VALUE sum;
		sum.b = atom.p->exec(parsed[0].b,parsed[1].b);
        for(int i = 2; i < atom.p->nargs; i++){
            sum.b = atom.p->exec(sum.b,parsed[i].b); 
        }
        free(parsed);
        return sum; 
    }

    #ifdef SECURE
    case SI : {
        DEBUG_PRINT("@Jumping to Insecure");
        
        // make Continue continuation 
        mystate->cont.empty = N(makeKCont)(mystate->env,mystate->cont.empty);

        // call the outside
        OTHERVALUE ptr; 
        ptr.b = insecure_eval((atom.i->arg.b)); 

        // No Heap
        if(ptr.tt == OTHERN(UNIT)){
            return convertin(ptr,atom.i->ty);
        }

        // Descriptors
        switch(ptr.b->t){

            case OTHERN(INT) : 
            case OTHERN(BOOLEAN) :
            case OTHERN(QUOTE) :
                return convertin(ptr,atom.i->ty);

            case OTHERN(LIST) :{
                VALUE v;

                VALUE * list = MALLOC(ptr.ls->nargs * (sizeof(VALUE))); 
                for(int i =0; i < ptr.ls->nargs; i++){
                   list[i] = convertin((ptr.ls->args[i]),atom.i->ty);// TODO fix
                }

                struct N(List) * data = MALLOC(sizeof(struct N(List)));
                data->t      = N(LIST);
                data->islist = ptr.ls->islist;
                data->nargs = ptr.ls->nargs;
                data->args = list;
                v.ls = data;

                return v;
            }
         
            case OTHERN(CLOSURE) : {

                TYPE left,right;
                function_types(atom.i->ty,&left,&right);

                // introduce the variable 'a at label
                VALUE * la = MALLOC(1 * sizeof(VALUE));
                la[0].b = N(makeSymbol)("a");
                
                // label -> 'a -> left  in Label bindings
                ANNOTATION * code = MALLOC(sizeof(ANNOTATION));
                code->t  = la[0];
                code->ty = left;
                int label = N(newLabel)();
                N(insertLabel)(&(mystate->label),label,code);
                DEBUG_PRINT("Adding Label (A) == %d",label);

                // apply closure to IS 'a     
                OTHERVALUE * ls = MALLOC(2 * sizeof(OTHERVALUE));
                ls[0].b = ptr.b;
                ls[1].b = makeIS(left.byte,label);
                void * appl = OTHERN(makeApplication)(2,ls);

                // λ 'a. SI t2 : (ptr.b (IS t1 : 'a))
				VALUE v; 
				v.b = N(makeLambda)(1,makeSI(right.byte,appl),la);
                return evalAtom(v);
            }

            default : break;
        }

        DEBUG_PRINT("Failed");
        exit(1);
    }
    #else
    case IS : {
        DEBUG_PRINT("@Jumping to Secure");

        VALUE val;

		// make Continue continuation
        mystate->cont.empty = N(makeKCont)(mystate->env,mystate->cont.empty);
		
        // Call the outside
        val.b = secure_eval(atom.i->label);  

        // TODO does this leak information ?
        if(val.b == NULL){
            DEBUG_PRINT("Invalid Label");
            exit(1);
        }
        
        return evalAtom(val); 
    }
    #endif

    case N(LAM) :{
		VALUE l;
		l.b =  N(makeClosure)(atom.b,mystate->env);
	    return l;
    }

    case N(QUOTE) :
        return atom.q->arg; 
        break;

    default :{ 
        DEBUG_PRINT("Not an Atom");
        exit(1);
    } 
   }
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    applyKont
 *  Description:    execute a continuation 
 * =====================================================================================
 */
LOCAL LIMBO applyKont(VALUE val,KONT k)
{
    if(k.empty == NULL){
        LIMBO ret = {.answer = val};
        return ret;
    }

    switch(k.l->t){

        case N(KLET) : { 
            struct N(KLet) * lk = k.l; 

            // let x = val
            VALUE * add = N(insertBinding)(&lk->e,lk->var.s->name);
            *add = val;
            
		    mystate->control  = lk->expr;
		    mystate->env      = lk->e;
		    mystate->cont     = lk->next;
            free(lk);
            LIMBO ret   = {NULL};
            return ret;
        }

        case N(KRET) :{
            mystate->cont    = k.r->next;     
            mystate->control = val;
            LIMBO ret  = {.answer = val};
            free(k.r);
            return ret;
        }

        case N(KCONTINUE) : {
            mystate->control = val;
            mystate->cont    = k.c->next;
            mystate->env     = k.c->e;
            free(k.c);
            LIMBO ret  = {NULL};
            return ret;
        }
        
        default :
            DEBUG_PRINT ("Unsupported Continuation");
            exit(1);
    }
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    apply
 *  Description:    apply arguments to produce a new VALUE
 * =====================================================================================
 */
LOCAL LIMBO apply(VALUE proc,VALUE * args){

    #ifdef DEBUG
    char * strd = N(toString)(proc,0);
    DEBUG_PRINT("CALL PROCEDURE %s",strd);
    free(strd);
    #endif

	if(proc.c->t == N(CLOSURE)){

        int nargs = proc.c->lambda.l->nargs; 

        #ifdef DEBUG
        DEBUG_PRINT(" STOP ");
        BINDING *node = proc.c->env;
        while(node){
        char * str =  N(toString)(*(node->address),0);
        DEBUG_PRINT("%s => %s ",node->key,str);
        node = node->next;
        free(str);
        }
        DEBUG_PRINT(" STOP ");
        #endif


		// forall xi of λ xi : xi = args[i]
        for(int i = 0; i < nargs ;i++){
            #ifdef DEBUG
            char * strc = N(toString)((args[i]),0);
            DEBUG_PRINT("Setting %s == %s",proc.c->lambda.l->arguments[i].s->name,strc);
            free(strc);
            #endif
            

            VALUE * add = N(insertBinding)(&proc.c->env,proc.c->lambda.l->arguments[i].s->name); 
            *add = args[i];
        }
        
		// create new state with updated storage and envorinment, control = body of lambda
        mystate->env = proc.c->env;
        mystate->control = (proc.c->lambda.l->body);; 
        LIMBO ret = {NULL};
        return ret;
	}
	else if(proc.k->t == N(CONTINUATION)){
        DEBUG_PRINT(">>>>> DOING CONTINUATION");
		return applyKont(args[0],proc.k->kstar);
	}
	else{
		DEBUG_PRINT("Unkown Procedure");
        exit(1);
	}
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    step
 *  Description:    execute a step in the CESK machine 
 * =====================================================================================
 */
LOCAL LIMBO step()
{
    // if the control is atomic
    if(isAtom(mystate->control))
    {
        VALUE return_val = evalAtom(mystate->control);
        return  applyKont(return_val,mystate->cont); 
    }

    // execute other control state 
    switch(mystate->control.b->t){

        case N(IF) : {
            VALUE condi = evalAtom(mystate->control.f->cond);	

		    if(condi.b->value == N(RTRUE)){
			    mystate->control = (mystate->control.f->cons);
		    }else{
			    mystate->control = (mystate->control.f->alt);;	
		    }

            LIMBO ret = {NULL};
		    return ret;
	    }

        case N(APPLICATION) : {
            VALUE * argum = MALLOC(sizeof(VALUE) * mystate->control.a->nargs);

            for(int i = 0; i < mystate->control.a->nargs; i++){
                argum[i] = evalAtom(mystate->control.a->arguments[i]);
            }

            VALUE * rest = argum + 1;
            LIMBO res = apply(argum[0],(rest));
            free(argum);
            return res;
        }

        case N(CALLCC) : {
            VALUE proc = evalAtom(mystate->control.cc->function);
            VALUE curr; 
			curr.b = N(makeContinuation)(mystate->cont.empty);
            LIMBO res = apply(proc,&curr);
            return res;
        }

        case N(SET) : {
            VALUE * address = N(getBinding)(mystate->env, mystate->control.sv->var.s->name); 
            VALUE val = evalAtom(mystate->control.sv->value);
            *address = val; 
            VALUE empty; 
			empty.b = N(makeNop());
            return applyKont(empty,mystate->cont);
        }

        case N(DEFINE) : {
            VALUE * address = N(getBinding)(mystate->env, mystate->control.d->var.s->name); 

            // If binding doesn't exist create one
            if(address == NULL) 
                address = N(insertBinding)(&mystate->env,mystate->control.d->var.s->name);

            VALUE val = evalAtom(mystate->control.d->expr);
            *address = (val); // MEM
            VALUE empty; 
			empty.b = N(makeNop());
            return applyKont(empty,mystate->cont);
        }

        case N(LET) : {
            VALUE a = (mystate->control.lt->expr);
            VALUE b = (mystate->control.lt->var);
            VALUE c = (mystate->control.lt->body);
            //sfreevalue(&(mystate->control));
            mystate->control = a;
            mystate->cont.empty = N(makeKLet)(b.b,c.b,mystate->env,mystate->cont.empty);
            LIMBO ret = {NULL};
		    return ret;
        }

        case N(LETREC) : {
            int nargs = mystate->control.lr->nargs; 

            // create all the variabels first -> they may be recursively intertwined in the x = a definitions
            VALUE ** list = MALLOC(sizeof(VALUE **) * nargs);
            for(int i = 0; i < nargs ; i++){
                list[i] =  N(insertBinding)(&mystate->env,mystate->control.lr->vars[i].s->name);                
            }

            // update locations
            for(int i = 0; i < nargs ; i++){ 
                *(list[i]) = evalAtom(mystate->control.lr->exprs[i]);
            }

            mystate->control = (mystate->control.lr->body);;
            LIMBO ret = {NULL};
		    return ret;
        } 

        case N(BEGIN) : {
            VALUE proc;
            for(int i = 0; i < mystate->control.bg->nargs; i++){ 
                proc = evalAtom(mystate->control.bg->stmts[i]);
            }
            return  applyKont(proc,mystate->cont); 
        }

        case N(CAR) : {
            struct N(Car) * here = mystate->control.car;
            VALUE val = evalAtom(here->arg);

            if(val.ls->t == N(LIST)){
                if (val.ls->nargs > 0){
                    return applyKont(val.ls->args[0],mystate->cont); 
                }else {
                    DEBUG_PRINT("Empty List!!");
                    exit(1);
                }
            }
            else{
                DEBUG_PRINT("Expected List");
                exit(1);
            }
        }

        case N(CDR) : {
            struct N(Cdr) * here = mystate->control.cdr;
            VALUE val = evalAtom(here->arg);

            if(val.ls->t == N(LIST)){
                if (val.ls->nargs > 1){
                    VALUE * newlist = MALLOC((val.ls->nargs-1) * (sizeof (VALUE))); 

                    for(int i = 1; i < val.ls->nargs; i++){
                        newlist[(i-1)] = (val.ls->args[i]); 
                    }

                    free(val.ls->args);
                    val.ls->args = newlist;
                    val.ls->nargs--;
                    return applyKont(val,mystate->cont); 
                }else if(val.ls->nargs == 1) {
					VALUE nn;
					nn.b = N(makeNIL)();
                    return applyKont(nn,mystate->cont);
                }
                else{
                    DEBUG_PRINT("Empty List!!");
                    exit(1);
                }
            }
            else{
                DEBUG_PRINT("Expected List");
                exit(1);
            }
        }

        case N(CONS) : {
            VALUE v = evalAtom(mystate->control.cons->arg);
            VALUE v2 = evalAtom(mystate->control.cons->arg2);

            if(v2.ls->t == N(LIST) && v2.ls->islist == 1){
                VALUE * newlist = MALLOC((v2.ls->nargs+1) * (sizeof (VALUE)));  
                newlist[0] = v;
                for(int i = 0; i < v2.ls->nargs; i++){
                    newlist[(i+1)] = (v2.ls->args[i]); 
                }

                free(v2.ls->args);
                v2.ls->args = newlist;
                v2.ls->nargs++;
                return applyKont(v2,mystate->cont);   
            }
			VALUE p;
			p.b  = N(makePair)(v.b,v2.b);
            return applyKont(p,mystate->cont);   
        }

        case N(PAIRQ) : {
            VALUE v = evalAtom(mystate->control.pq->arg);  

            if(v.ls->t == N(LIST)){
                if(v.ls->islist == 1){
					VALUE p;
					p.b = N(makeBoolean)((v.ls->nargs > 0));
                    return applyKont(p,mystate->cont);  
                }
				VALUE nn;
				nn.b = N(makeBoolean)((v.ls->nargs > 1));
                return applyKont(nn,mystate->cont);  
            }
            else{
                DEBUG_PRINT("Expected List");
                exit(1);
            }
        }

        case N(LISTQ) : {
            VALUE v = evalAtom(mystate->control.lq->arg);  
            if(v.ls->t == N(LIST)){
				VALUE p;
				p.b = N(makeBoolean)(v.ls->islist);
                return applyKont(p,mystate->cont); 
            }
            else{
                DEBUG_PRINT("Expected List");
                exit(1);
            }
        }   
    
        case N(NULLQ) : {
            VALUE v = evalAtom(mystate->control.nq->arg);  
            if(v.ls->t == N(LIST)){
				VALUE p;
				p.b = N(makeBoolean)(v.ls->nargs == 0);
                return applyKont(p,mystate->cont); 
            }
            else{
                DEBUG_PRINT("Expected List");
                exit(1);
            }
        }   

        default :
		    DEBUG_PRINT("Unkown State");
            exit(1);
            break;

    }
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    steprec
 *  Description:    step through program step by step
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(steprec)(){

    #ifdef DEBUG

    #ifdef SECURE
    char * str = "SECURE";
    #else
    char * str = "INSECURE";
    #endif

    static int i = 0;
    DEBUG_PRINT("STEP# == %d of %s",++i,str);
    int log = i;

    debugState();
    #endif

    // calculate
    LIMBO result = step(); 

    #ifdef DEBUG
    DEBUG_PRINT("DONE Secure STEP#== %d of %s",log,str);
    #endif

    if(result.empty == NULL){
        return N(steprec)();
    }
    else{
        return result.answer;
    }
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    inject
 *  Description:    create a new start state
 * =====================================================================================
 */
FUNCTIONALITY void N(inject)(){

    if(mystate != NULL) { free(mystate); }

    // inject state
    mystate             = MALLOC(sizeof(STATE));
    mystate->env        = NULL;
    mystate->cont.empty = NULL;
    #ifdef SECURE
    mystate->label      = NULL;
    #endif
	mystate->free_adr   = 0;
}

#ifdef SECURE


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    secure_eval
 *
 *     Argument:    <label> 
 *                  A label to a Ruse ML/Scheme function that was either: 
 *                      - statically defined
 *                      - dynamically released
 *
 *  Description:    executes the Ruse Ml/Scheme function that the label points to
 *
 * Precondition:    If either of the following preconditions are violated the 
 *                  RUSE AM shuts down :
 *                      - sload must be executed
 *                      - Argument must  
 *
 *       Result:    A Ruse ML / Scheme VALUE in bytes in unprotected memory
 * =====================================================================================
 */
ENTRYPOINT void * secure_eval(int label){

    
    DEBUG_PRINT("Check :: that load has been executed");

    if(!load){
        DEBUG_PRINT("Load must be called first !!");
        exit(1);
    }

    DEBUG_PRINT("Check :: label == %d",label);
    
    ANNOTATION * code = N(hasLabel)(mystate->label,label);
    if(code != NULL){
        mystate->control = code->t;
        DEBUG_PRINT("Check Succeeded");

        // make Return Continuation
        mystate->cont.empty = N(makeKRet)(mystate->cont.empty);
        
        // compute
        VALUE in  = N(steprec)();
        TYPE goal = code->ty;

        // No Heap
        switch(in.tt){
            case N(UNIT) :
                return OTHERN(makeUnit)();
            case N(NOP) :
                return OTHERN(makeNop)();

            default : break;
            
        }
    
        // Descriptors
        switch(in.b->t){

            case N(QUOTE)   : 
            case N(BOOLEAN) : 
            case N(INT)     : 
                return convertout(in,goal);

            case N(LIST) : {
                OTHERVALUE v;

                OTHERVALUE * list = MALLOC(in.ls->nargs * (sizeof(OTHERVALUE))); 
                for(int i =0; i < in.ls->nargs; i++){
                    list[i].b = convertout(in.ls->args[i],goal); // TODO fix   
                }

                struct OTHERN(List) * data = MALLOC(sizeof(struct OTHERN(List)));
                data->t      = OTHERN(LIST);
                data->islist = in.ls->islist;
                data->nargs = in.ls->nargs;
                data->args = list;
                v.ls = data;
                return v.b;
            }

            case N(CLOSURE) : {

                TYPE left,right; 
                function_types(goal,&left,&right);
                
                
                // introduce the variable 'z
                OTHERVALUE * la = MALLOC(1 * sizeof(OTHERVALUE));
                la[0].b = OTHERN(makeSymbol)("z");

                // apply closure to SI 'z 
                VALUE * ls = MALLOC(2 * sizeof(VALUE));
                ls[0].b = in.b;
                ls[1].b = makeSI(left.byte,la[0].b); // TODO typecheck
                
                // add the closure to the storage position at c + new label
                ANNOTATION * code = MALLOC(sizeof(ANNOTATION));
                code->ty = right;
                code->t.b  = N(makeApplication)(2,ls);
                int label = N(newLabel)();
                N(insertLabel)(&(mystate->label),label,code);
                DEBUG_PRINT("Return :: Adding Label == %d",label);
                
                return (OTHERN(makeLambda)(1,makeIS(right.byte,label),la)); 
            }

            default : break;
        }

        DEBUG_PRINT("Invalid Return Value");
        exit(1); 
    }

    DEBUG_PRINT("Requires a valid Label");
    exit(1);
}

#ifndef BYTE
HOOK char N(input_byte)[];
#endif


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    sload
 *
 *     Argument:    <name>
 *                  The name of a secure program. This value is currently ignored :
 *                      - It's best to just pass NULL
 *
 *                  <callback>
 *                  The evaluator to be called by the secure_eval method when wanting 
 *                  to compute insecure code.
 *
 *  Description:   setup the machine  
 *
 * =====================================================================================
 */
ENTRYPOINT void sload(char * strbuf,void * (*callback)(void *)){

    // sancus config
    #ifdef SANCUS_SPM
    WDTCTL = WDTPW | WDTHOLD;
    #ifdef FPGA
    uart_init();
    #endif
    protect_sm(&secure_vm); // todo this is misplaced
    #endif

    #ifdef SANCUS_SPM
    printf("started secure component\n");
    #endif

    N(inject)();
    int l = 0;
    #ifdef BYTE
    ANNOTATION ** code = N(readByteCode)(strbuf,&l);
    #else
    ANNOTATION ** code = N(readByteCode)(N(input_byte),&l);
    #endif

    for(int i = 0; i < l; i++){
        int label = N(newLabel)();
        N(insertLabel)(&(mystate->label),label,code[i]);
    }

    // set up callback
    insecure_eval = callback;

    // load is done
    load = 1;
}
#endif


#ifndef SECURE

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    evaluate
 *
 *     Argument:    <code> 
 *                  Bytes convertible into VALUE structs that are to be executed
 *                  by the evaluator
 *
 *  Description:    executes the Ruse Ml/Scheme code that the bytes represent
 *
 * Precondition:    If the following preconditions is violated the RUSE AM shuts down :
 *                      - code must be convertible into a valid VALUE
 *
 *       Result:    A Ruse ML / Scheme VALUE in bytes in unprotected memory
 * =====================================================================================
 */
HOOK void * evaluate(void * code){ 

	// make Return continuation
    mystate->cont.empty = N(makeKRet)(mystate->cont.empty);
    mystate->control.b = code;

    // compute
    VALUE ans  = N(steprec)();
	return ans.b;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    run
 *  Description:    run the program  
 * =====================================================================================
 */
HOOK void run (VALUE * program,int c){

    mystate->control = program[0];

    for(int i = 0; i < c ; i++){
        VALUE ans = N(steprec)(); 
        if(ans.tt != N(NOP)){
            char * result = N(toString)(ans,1);
            printf("%s\n",result);
            free(result);
        }
        mystate->control = program[(i+1)%c]; 
    }
    free(program);
    // TODO free binding
    free(mystate);
}

#endif

