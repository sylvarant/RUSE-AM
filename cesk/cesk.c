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

#include <stdlib.h>
#include <stdbool.h>
#include <stdarg.h>

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
enum{NUM_ELEMS = 1024};

/*-----------------------------------------------------------------------------
 *  Local Functions
 *-----------------------------------------------------------------------------*/

LOCAL VALUE steprec (void);
LOCAL LIMBO step(void);
LOCAL void inject(void);
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

    DEBUG_PRINT(("==========================")) 
    DEBUG_PRINT(("** CONTROL")) 

    char * ctrl = N(toString)(mystate->control,false);
    DEBUG_PRINT((ctrl)) 
    free(ctrl);
    DEBUG_PRINT(("** STORES : %d",mystate->free_adr)) 
    for(int i = 0; i < mystate->free_adr; i++){
        char * str =  N(toString)(mystate->storage[i],false);
        DEBUG_PRINT(("%d == %s",i,str))
        free(str);
    }
    DEBUG_PRINT(("** ENVIRONMENT : ")) 
    BINDING *node = mystate->env;
    while(node){
        DEBUG_PRINT(("%s at %d ",node->key,node->value))
        node = node->next;
    }
    DEBUG_PRINT(("** CONTINUATION")) 
    VALUE cc = N(makeContinuation)(mystate->cont);
    DEBUG_PRINT((" --> cont type %s",N(toString)(cc,false)))

    #ifdef SECURE
    DEBUG_PRINT(("** FUNCTIONS")) 
    Label * snode = mystate->label;
    while(snode != NULL){ 
        DEBUG_PRINT(("--> Label == %d",snode->label))
        snode = snode->next;
    }
    #endif
    DEBUG_PRINT(("==========================")) 
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
    enum STag atoms[] = {   N(LAM),N(INT),N(SYMBOL),N(BOOLEAN),N(PRIM),
                            N(LIST),N(QUOTE),N(CLOSURE),
                            #ifdef SECURE
                                SI
                            #else
                                IS
                            #endif
                        };
    if(el.tt == N(VOID)) return 1;
    for(int i = 0; i < NELEMS(atoms); i++){ 
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

        case N(VOID) :
            return atom;
    }

    switch(atom.b->t){

    case N(SYMBOL) :{
        int adress = (int) N(getBinding)(mystate->env,(const char *) atom.s->name); 

        if(adress == -1){ 
            DEBUG_PRINT(("Storage failure for %s",atom.s->name))
            DEBUG_PRINT(("ENVIRONMENT : ")) 
            BINDING *node = mystate->env;
            while(node){
                DEBUG_PRINT(("%s at %d ",node->key,node->value))
                node = node->next;
            }
            exit(1);
        }

        VALUE res = mystate->storage[adress]; 
        if(res.tt == N(ERROR)) {
            DEBUG_PRINT(("Unintialized Binding to %s",atom.s->name))
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
        DEBUG_PRINT(("Starting Primitive"))
        VALUE * parsed = MALLOC(atom.p->nargs * sizeof(VALUE));
        for(int i = 0; i < atom.p->nargs; i++){
            parsed[i] = evalAtom(atom.p->arguments[i]);
        }
        VALUE sum = atom.p->exec(parsed[0],parsed[1]);
        for(int i = 2; i < atom.p->nargs; i++){
            sum = atom.p->exec(sum,parsed[i]); 
        }
        free(parsed);
        DEBUG_PRINT(("Ending Primitive"))
        return sum; 
    }

    #ifdef SECURE
    case SI : {
        DEBUG_PRINT(("@Jumping to Insecure"))
        
        // make Continue continuation 
        mystate->cont = N(makeKCont)(mystate->env,mystate-cont);

        // call the outside
        OTHERVALUE ptr = evaluate((atom.i->arg)); 

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
                DEBUG_PRINT(("Adding Label (A) == %d",c))
                mystate->free_adr++;
                return evalAtom(N(makeLambda)(1,makeSI(OTHERN(makeApplication)(2,ptr,makeIS(c))),N(makeSymbol)("a")));
            }
        }

        DEBUG_PRINT(("Failed"))
        exit(1);
    }
    #else
    case IS : {
        DEBUG_PRINT(("@Jumping to Secure"))

        VALUE val;

		// make Continue continuation
        mystate->cont = N(makeKCont)(mystate->env,mystate-cont);
		
        // Call the outside
        val.b = secure_eval(atom.i->label);  

        // TODO does this leak information ?
        if(val.b == -1){
            DEBUG_PRINT(("Invalid Label"))
            exit(1);
        }
        
        return evalAtom(val); 
    }
    #endif

    case N(LAM) :{
        VALUE cpy = N(copyValue)(atom); 
	    return N(makeClosure)(cpy,mystate->env);
    }


    case N(QUOTE) :
        return atom.q->arg; 
        break;

    default :{ 
        DEBUG_PRINT(("Not to be used"))
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
		    mystate->storage[mystate->free_adr] = N(copyValue)(val); // MEM : Don't clear  
		    mystate->free_adr++;
            //sfreevalue(&mystate->control);  
		    mystate->control  = lk->expr;
		    mystate->env      = lk->e;
		    mystate->cont     = lk->next;
            LIMBO ret   = {NULL};
            return ret;
        }

        case SKRET :{
            mystate->cont    = k.r->next;     
            mystate->control = val;
            LIMBO ret  = {.answer = val};
            return ret;
        }

        case SKCONTINUE : {
            mystate->control = val;
            mystate->cont    = k.c->next;
            mystate->env     = k.c->e;
            LIMBO ret  = {NULL};
            return ret;
        }
        
        default :
            DEBUG_PRINT (("Unsupported Continuation"))
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

    DEBUG_PRINT(("CALL PROCEDURE %s",N(toString)(proc,false)))
	if(proc.c->t == N(CLOSURE)){

        int curr = mystate->free_adr;
        int nargs = proc.c->lambda.l->nargs; 
        DEBUG_PRINT(("Proc arg == %d",nargs))
        mystate->free_adr = curr + nargs; 

		// update enviroment with new adresses for each variable of lambda
        for(int j = curr,i = 0; j < mystate->free_adr ;j++){
            N(insertBinding)(&proc.c->env,proc.c->lambda.l->arguments[i].s->name,j); 
            i++;
        }

		// update storage with adresses pointing to arguments
        for(int j = curr,i = 0; j < mystate->free_adr ;j++){
            DEBUG_PRINT(("Proc %d == %s",i,N(toString)((args[i]),false)))
            mystate->storage[j] = N(copyValue)(args[i]); // MEM : Don't clear
            i++;
        }
        
		// create new state with updated storage and envorinment, control = body of lambda
        mystate->env = N(copyBinding)(proc.c->env);
        VALUE cpy = N(copyValue)(proc.c->lambda.l->body);
        //sfreevalue(&mystate->control);
        mystate->control = cpy; 
        LIMBO ret = {NULL};
        return ret;
	}
	else if(proc.k->t == N(CONTINUATION)){
        DEBUG_PRINT((">>>>> DOING CONTINUATION"))
		return applyKont(args[0],proc.k->kstar);
	}
	else{
		DEBUG_PRINT(("Unkown Procedure"))
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
                VALUE cpy = N(copyValue)(mystate->control.f->cons);
                //sfreevalue(&mystate->control);
			    mystate->control = cpy;
		    }else{
                VALUE cpy = N(copyValue)(mystate->control.f->alt);
                //sfreevalue(&mystate->control);
			    mystate->control = cpy;	
		    }

            LIMBO ret = {NULL};
		    return ret;
	    }

        case N(APPLICATION) : {
            DEBUG_PRINT(("Appl arg == %d",mystate->control.a->nargs))
            VALUE * argum = MALLOC(sizeof(VALUE) * mystate->control.a->nargs);

            for(int i = 0; i < mystate->control.a->nargs; i++){
                argum[i] = evalAtom(mystate->control.a->arguments[i]);
            }

            LIMBO res = apply(argum[0],(++argum));
            free(--argum);
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
            int adress = N(getBinding)(mystate->env,(const char *) mystate->control.sv->var.s->name); 
            FREECELL((mystate->storage[adress]))
            mystate->storage[adress] = N(copyValue)(val); // MEM 
            VALUE empty = N(makeVoid());
            return applyKont(empty,mystate->cont);
        }

        case N(DEFINE) : {
        
            // TODO keep ?
            if( mystate->control.d->var.s->t != N(SYMBOL)) {DEBUG_PRINT(("Expected Symbol !!")) exit(1);}
            int test = (int) N(getBinding)(mystate->env,(const char *) mystate->control.d->var.s->name); 

            if(test == -1){ 
                N(insertBinding)(&mystate->env,(const char *)mystate->control.d->var.s->name,mystate->free_adr++);
            }        

            // once binding exists preform set
            VALUE val = evalAtom(mystate->control.d->expr);
            int adress = (int) N(getBinding)(mystate->env,(const char *) mystate->control.d->var.s->name); 
            FREECELL((mystate->storage[adress]))
            mystate->storage[adress] = N(copyValue)(val); // MEM
            VALUE empty = N(makeVoid());
            return applyKont(empty,mystate->cont);
        }

        case N(LET) : {
            VALUE a = N(copyValue)(mystate->control.lt->expr);
            VALUE b = N(copyValue)(mystate->control.lt->var);
            VALUE c = N(copyValue)(mystate->control.lt->body);
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
                N(insertBinding)(&mystate->env,(const char *)mystate->control.lr->vars[i].s->name,j); 
                i++;
            }

            VALUE * list = MALLOC(mystate->control.lr->nargs * (sizeof(VALUE)));

            // evaluate
            for(int i = 0; i < mystate->control.lr->nargs ;i++){
                list[i] = evalAtom(mystate->control.lr->exprs[i]);
            }

		    // update storage with adresses pointing to arguments
            for(int j = curr,i = 0; j < mystate->free_adr ;j++,i++){
                mystate->storage[j] = N(copyValue)(list[i]); // MEM : new adress do not delete
            }

            VALUE b = N(copyValue)(mystate->control.lr->body);
            //sfreevalue(&(mystate->control));
            mystate->control = b;
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
                    DEBUG_PRINT(("Empty List!!"))
                    exit(1);
                }
            }
            else{
                DEBUG_PRINT(("Expected List"))
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
                        newlist[(i-1)] = N(copyValue)(val.ls->args[i]); 
                    }

                    free(val.ls->args);
                    val.ls->args = newlist;
                    val.ls->nargs--;
                    return applyKont(val,mystate->cont); 
                }else if(val.ls->nargs == 1) {
                    return applyKont(N(makeNIL()),mystate->cont);
                }
                else{
                    DEBUG_PRINT(("Empty List!!"))
                    exit(1);
                }
            }
            else{
                DEBUG_PRINT(("Expected List"))
                exit(1);
            }
        }

        case N(CONS) : {
            VALUE v = evalAtom(mystate->control.cons->arg);
            VALUE v2 = evalAtom(mystate->control.cons->arg2);

            if(v2.ls->t == N(LIST) && v2.ls->islist == true){
                VALUE * newlist = MALLOC((v2.ls->nargs+1) * (sizeof (VALUE)));  
                newlist[0] = v;
                for(int i = 0; i < v2.ls->nargs; i++){
                    newlist[(i+1)] = N(copyValue)(v2.ls->args[i]); 
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
                if(v.ls->islist == true){
                    return applyKont(N(makeBoolean)((v.ls->nargs > 0)),mystate->cont);  
                }
                return applyKont(N(makeBoolean)((v.ls->nargs > 1)),mystate->cont);  
            }
            else{
                DEBUG_PRINT(("Expected List"))
                exit(1);
            }
        }

        case N(LISTQ) : {
            VALUE v = evalAtom(mystate->control.lq->arg);  
            if(v.ls->t == N(LIST)){
                return applyKont(N(makeBoolean)(v.ls->islist),mystate->cont); 
            }
            else{
                DEBUG_PRINT(("Expected List"))
                exit(1);
            }
        }   
    
        case N(NULLQ) : {
            VALUE v = evalAtom(mystate->control.nq->arg);  
            if(v.ls->t == N(LIST)){
                return applyKont(N(makeBoolean)(v.ls->nargs == 0),mystate->cont); 
            }
            else{
                DEBUG_PRINT(("Expected List"))
                exit(1);
            }
        }   

        default :
		    DEBUG_PRINT(("Unkown State"))
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
LOCAL VALUE steprec (){

    static int i = 0;
    DEBUG_PRINT(("STEP# == %d of Secure",++i))
    int log = i;

    // output state
    #ifdef DEBUG
        debugState();
    #endif

    // calculate
    LIMBO result = step(); 
    DEBUG_PRINT(("DONE Secure STEP#== %d",log))

    if(result.empty == NULL){
        return steprec();
    }
    else{
        return result.answer;
    }
}


#ifdef SECURE

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    inject
 *  Description:    create a new start state
 * =====================================================================================
 */
LOCAL void inject (){

    if(mystate != NULL) { free(mystate); }

    // inject state
    mystate             = MALLOC(sizeof(STATE));
    mystate->env        = NULL;
    mystate->storage    = MALLOC(NUM_ELEMS * sizeof(VALUE)); 
    mystate->cont.empty = NULL;
    mystate->label      = NULL;
	mystate->free_adr   = 0;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    secure_eval
 *  Description:    the entry point for the outside
 * =====================================================================================
 */
ENTRYPOINT void * secure_eval(int label){

    DEBUG_PRINT(("Check :: label == %d",label))

    if(hasLabel(mystate->label,label)){
        mystate->control = mystate->storage[label];
        DEBUG_PRINT(("Check Succeeded"))

        // make Return Continuation
        mystate->cont = N(makeKRet)(mystate->cont);
        
        // compute
        VALUE in = steprec();

        // No Heap
        if(in.tt == N(VOID)){
            Value empty = {0};
            return empty.b;
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
                    DEBUG_PRINT(("Adding Label (A) == %d",d))
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
                DEBUG_PRINT(("Return :: Adding Label == %d \\ \t for member %s",c,N(toString)(mystate->storage[c],false)))
                return (OTHERN(makeLambda)(1,makeIS(c),OTHERN(makeSymbol)("z"))).b; 
            }
        }

        DEBUG_PRINT(("Invalid Return Value"))
        exit(1); 
    }

    return -1;
}
#endif

/*
REPLACE */

