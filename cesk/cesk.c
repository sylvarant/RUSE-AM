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
#include "string.h"

/*-----------------------------------------------------------------------------
 *  Preprocessor
 *-----------------------------------------------------------------------------*/

#define NELEMS(x)  (sizeof(x) / sizeof(x[0]))
#define FREECELL(y) do{} while(0); // if(y.b != NULL) sfreeValue(&y);


/*-----------------------------------------------------------------------------
 *  Local Constants
 *-----------------------------------------------------------------------------*/
enum{NUM_ELEMS = 128};

/*-----------------------------------------------------------------------------
 *  Local Functions
 *-----------------------------------------------------------------------------*/

LOCAL LIMBO step(void);
LOCAL LIMBO apply(VALUE proc,VALUE * args);
LOCAL LIMBO applyKont(VALUE val,N(Kont) k);
LOCAL VALUE evalAtom(VALUE atom);
LOCAL unsigned int isAtom(VALUE atom);


/*-----------------------------------------------------------------------------
 *  File Members
 *-----------------------------------------------------------------------------*/

// this is the state of the cesk machine,
// which is local to the file for security reasons
SECRET_DATA STATE * mystate = NULL;


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
    DEBUG_PRINT("** STORES : %d",mystate->free_adr); 
    for(int i = 0; i < mystate->free_adr; i++){
        char * str =  N(toString)(mystate->storage[i],0);
        DEBUG_PRINT("%d == %s",i,str);
        free(str);
    }
    DEBUG_PRINT("** ENVIRONMENT : "); 
    BINDING *node = mystate->env;
    while(node){
        DEBUG_PRINT("%s at %d ",node->key,node->value);
        node = node->next;
    }
    DEBUG_PRINT("** CONTINUATION"); 
    VALUE cc = N(makeContinuation)(mystate->cont);
    char * strc = N(toString)(cc,0);
    DEBUG_PRINT(" --> cont type %s",strc);
    free(strc);

    #ifdef SECURE
    DEBUG_PRINT("** FUNCTIONS"); 
    Label * snode = mystate->label;
    while(snode != NULL){ 
        DEBUG_PRINT("--> Label == %d",snode->label);
        snode = snode->next;
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
    if(el.tt == N(VOID) || el.tt == N(NOP)) return 1;
    for(unsigned int i = 0; i < NELEMS(atoms); i++){ 
        if(el.b->t == atoms[i]){return 1;}
    }
    return 0;
}
    

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
        case N(VOID) :
            return atom;

        default : break;
    }

    switch(atom.b->t){

    case N(SYMBOL) :{
        int adress = (int) N(getBinding)(mystate->env,atom.s->name); 

        if(adress == -1){ 
            DEBUG_PRINT("Storage failure for %s",atom.s->name);
            DEBUG_PRINT("ENVIRONMENT : "); 
            BINDING *node = mystate->env;
            while(node){
                DEBUG_PRINT("%s at %d ",node->key,node->value);
                node = node->next;
            }
            exit(1);
        }

        VALUE res = mystate->storage[adress]; 
        if(res.tt == N(ERROR)) {
            DEBUG_PRINT("Unintialized Binding to %s",atom.s->name);
            exit(1);
        }
        return res;
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
            parsed[i] = evalAtom(atom.p->arguments[i]);
        }
        VALUE sum = atom.p->exec(parsed[0],parsed[1]);
        for(int i = 2; i < atom.p->nargs; i++){
            sum = atom.p->exec(sum,parsed[i]); 
        }
        free(parsed);
        return sum; 
    }

    #ifdef SECURE
    case SI : {
        DEBUG_PRINT("@Jumping to Insecure");
        
        // make Continue continuation 
        mystate->cont = N(makeKCont)(mystate->env,mystate->cont);

        // call the outside
        OTHERVALUE ptr; 
        ptr.b = evaluate((atom.i->arg.b)); 

        // No Heap
        if(ptr.tt == OTHERN(VOID)){
            VALUE empty = {0};
            return empty;
        }

        // Descriptors
        switch(ptr.b->t){

            case OTHERN(LIST) :{
                VALUE v;

                VALUE * list = MALLOC(ptr.ls->nargs * (sizeof(VALUE))); 
                for(int i =0; i < ptr.ls->nargs; i++){
                   list[i] = makeSI(ptr.ls->args[i]);   
                }

                struct N(List) * data = MALLOC(sizeof(struct N(List)));
                data->t      = N(LIST);
                data->islist = ptr.ls->islist;
                data->nargs = ptr.ls->nargs;
                data->args = list;
                v.ls = data;

                return v;
            }
         
            case OTHERN(BOOLEAN) :{
                return N(makeBoolean)(ptr.b->value);
            }

            case OTHERN(INT) :{ 
                return N(makeInt)(ptr.b->value);
            }

            case OTHERN(CLOSURE) : {
                int c = mystate->free_adr;
                mystate->storage[mystate->free_adr] = N(makeSymbol)("a");
                insertLabel(&(mystate->label),mystate->free_adr);
                DEBUG_PRINT("Adding Label (A) == %d",c);
                mystate->free_adr++;
                return evalAtom(N(makeLambda)(1,makeSI(OTHERN(makeApplication)(2,ptr,makeIS(c))),N(makeSymbol)("a")));
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
        mystate->cont = N(makeKCont)(mystate->env,mystate->cont);
		
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
	    return N(makeClosure)(atom,mystate->env);
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
            N(insertBinding)(&lk->e,lk->var.s->name,mystate->free_adr);
		    mystate->storage[mystate->free_adr] = (val); // MEM : Don't clear  
		    mystate->free_adr++;
            
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

        int curr = mystate->free_adr;
        int nargs = proc.c->lambda.l->nargs; 
        DEBUG_PRINT("Proc arg == %d",nargs);
        mystate->free_adr = curr + nargs; 

		// update enviroment with new adresses for each variable of lambda
        for(int j = curr,i = 0; j < mystate->free_adr ;j++){
            N(insertBinding)(&proc.c->env,proc.c->lambda.l->arguments[i].s->name,j); 
            i++;
        }

		// update storage with adresses pointing to arguments
        for(int j = curr,i = 0; j < mystate->free_adr ;j++){
            #ifdef DEBUG
            char * strc = N(toString)((args[i]),0);
            DEBUG_PRINT("Proc %d == %s",i,strc);
            free(strc);
            #endif
            mystate->storage[j] = (args[i]); // MEM : Don't clear
            i++;
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

		    if(condi.b->value == 1){
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
            VALUE curr = N(makeContinuation)(mystate->cont);
            LIMBO res = apply(proc,&curr);
            return res;
        }

        case N(SET) : {
            VALUE val = evalAtom(mystate->control.sv->value);
            int adress = N(getBinding)(mystate->env, mystate->control.sv->var.s->name); 
            FREECELL((mystate->storage[adress]))
            mystate->storage[adress] = val; 
            VALUE empty = N(makeNop());
            return applyKont(empty,mystate->cont);
        }

        case N(DEFINE) : {
        
            // TODO keep ?
            if( mystate->control.d->var.s->t != N(SYMBOL)) {DEBUG_PRINT("Expected Symbol !!"); exit(1);}
            int test = (int) N(getBinding)(mystate->env, mystate->control.d->var.s->name); 

            if(test == -1){ 
                N(insertBinding)(&mystate->env,mystate->control.d->var.s->name,mystate->free_adr++);
            }        

            // once binding exists preform set
            VALUE val = evalAtom(mystate->control.d->expr);
            int adress = (int) N(getBinding)(mystate->env, mystate->control.d->var.s->name); 
            FREECELL((mystate->storage[adress]))
            mystate->storage[adress] = (val); // MEM
            VALUE empty = N(makeNop());
            return applyKont(empty,mystate->cont);
        }

        case N(LET) : {
            VALUE a = (mystate->control.lt->expr);
            VALUE b = (mystate->control.lt->var);
            VALUE c = (mystate->control.lt->body);
            //sfreevalue(&(mystate->control));
            mystate->control = a;
            mystate->cont = N(makeKLet)(b,c,mystate->env,mystate->cont);
            LIMBO ret = {NULL};
		    return ret;
        }

        case N(LETREC) : {
            int curr = mystate->free_adr;
            int nargs = mystate->control.lr->nargs; 
            mystate->free_adr = curr + nargs; 

		    // update enviroment with new adresses for each variable of lambda
            for(int j = curr,i = 0; j < mystate->free_adr ;j++){
                N(insertBinding)(&mystate->env,mystate->control.lr->vars[i].s->name,j); 
                i++;
            }

            VALUE * list = MALLOC(mystate->control.lr->nargs * (sizeof(VALUE)));

            // evaluate
            for(int i = 0; i < mystate->control.lr->nargs ;i++){
                list[i] = evalAtom(mystate->control.lr->exprs[i]);
            }

		    // update storage with adresses pointing to arguments
            for(int j = curr,i = 0; j < mystate->free_adr ;j++,i++){
                mystate->storage[j] = (list[i]); // MEM : new adress do not delete
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
                    return applyKont(N(makeNIL()),mystate->cont);
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
            return applyKont(N(makePair)(v,v2),mystate->cont);   
        }

        case N(PAIRQ) : {
            VALUE v = evalAtom(mystate->control.pq->arg);  

            if(v.ls->t == N(LIST)){
                if(v.ls->islist == 1){
                    return applyKont(N(makeBoolean)((v.ls->nargs > 0)),mystate->cont);  
                }
                return applyKont(N(makeBoolean)((v.ls->nargs > 1)),mystate->cont);  
            }
            else{
                DEBUG_PRINT("Expected List");
                exit(1);
            }
        }

        case N(LISTQ) : {
            VALUE v = evalAtom(mystate->control.lq->arg);  
            if(v.ls->t == N(LIST)){
                return applyKont(N(makeBoolean)(v.ls->islist),mystate->cont); 
            }
            else{
                DEBUG_PRINT("Expected List");
                exit(1);
            }
        }   
    
        case N(NULLQ) : {
            VALUE v = evalAtom(mystate->control.nq->arg);  
            if(v.ls->t == N(LIST)){
                return applyKont(N(makeBoolean)(v.ls->nargs == 0),mystate->cont); 
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
        return steprec();
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
    mystate->storage    = MALLOC(NUM_ELEMS * sizeof(VALUE)); 
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
 *  Description:    the entry point for the outside
 * =====================================================================================
 */
ENTRYPOINT void * secure_eval(int label){

    DEBUG_PRINT("Check :: label == %d",label);

    if(hasLabel(mystate->label,label)){
        mystate->control = mystate->storage[label];
        DEBUG_PRINT("Check Succeeded");

        // make Return Continuation
        mystate->cont = N(makeKRet)(mystate->cont);
        
        // compute
        VALUE in = steprec();

        // No Heap
        switch(in.tt){
            case N(VOID) :
                return OTHERN(makeVoid)().b;
            case N(NOP) :
                return OTHERN(makeNop)().b;

            default : break;
            
        }
    
        // Descriptors
        switch(in.b->t){

            case N(LIST) : {
                OTHERVALUE v;

                OTHERVALUE * list = MALLOC(in.ls->nargs * (sizeof(OTHERVALUE))); 
                for(int i =0; i < in.ls->nargs; i++){
                    int d = mystate->free_adr; 
                    mystate->storage[mystate->free_adr] = in.ls->args[i];
                    insertLabel(&(mystate->label),mystate->free_adr);
                    DEBUG_PRINT("Adding Label (A) == %d",d);
                    mystate->free_adr++;
                    list[i] = makeIS(d);   
                }

                struct OTHERN(List) * data = MALLOC(sizeof(struct OTHERN(List)));
                data->t      = OTHERN(LIST);
                data->islist = in.ls->islist;
                data->nargs = in.ls->nargs;
                data->args = list;
                v.ls = data;
                return v.b;
            }

            case N(BOOLEAN) : {
                return (OTHERN(makeBoolean)(in.b->value)).b; 
            }

            case N(INT) : {
                return (OTHERN(makeInt)(in.z->value)).b; 
            }

            case N(CLOSURE) : {
                int c = mystate->free_adr;
                mystate->free_adr++;
                insertLabel(&(mystate->label),c);
                mystate->storage[c] = N(makeApplication)(2,in,makeSI(OTHERN(makeSymbol)("z")));
                #ifdef DEBUG
                char * strc = N(toString)(mystate->storage[c],0);
                DEBUG_PRINT("Return :: Adding Label == %d \t for member %s",c,strc);
                free(strc);
                #endif
                return (OTHERN(makeLambda)(1,makeIS(c),OTHERN(makeSymbol)("z"))).b; 
            }

            default : break;
        }

        DEBUG_PRINT("Invalid Return Value");
        exit(1); 
    }

    return NULL;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    sload
 *  Description:    setup the secure module -> todo high up
 * =====================================================================================
 */
ENTRYPOINT void sload(char * strbuf){
    N(inject)();
    VALUE * ignore = N(readByteCode)(mystate,strbuf);
}
#endif


#ifndef SECURE

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    evaluate
 *  Description:    main entry point for the outside
 *					argument is a pointer to deal with Sancus restriction
 * =====================================================================================
 */
HOOK void * evaluate(void * v){ 

	// make Return continuation
    mystate->cont = N(makeKRet)(mystate->cont);
    mystate->control.b = v;

    // compute
    VALUE ans  = steprec();
	return ans.b;
}

#endif

