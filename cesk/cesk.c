/*
 * =====================================================================================
 *
 *       Filename:  cesk.c
 *
 *    Description:  Cesk machine in c
 *
 *        Version:  1.0
 *        Created:  10/17/2012 14:54:40
 *       Compiler:  gcc
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */


/*-----------------------------------------------------------------------------
 *  Preprocessor
 *-----------------------------------------------------------------------------*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdarg.h>
#include <math.h>
#include "scheme.h"
#include "global.h"

#define MEM_SIZE 1024
#define NELEMS(x)  (sizeof(x) / sizeof(x[0]))
#define FREECELL(y) do{} while(0); // if(y.b != NULL) freevalue(&y);


/*-----------------------------------------------------------------------------
 *  Types
 *-----------------------------------------------------------------------------*/

// what is what 
typedef Value * memory;

// CESK State
typedef struct state_t{
    Value control;
    environ * environment; 
    memory storage;
    kont continuation;
	int free_adr;
}state;

typedef struct answer_t{
    Value ans;
    state * s;
}answer;

// state + answer
typedef struct limbo_t{
    state * computation;
    answer ans;
}limbo;

state * mystate = NULL;

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    debugstate
 *  Description:    print the current cesk state
 * =====================================================================================
 */
static void debugstate(state * s){
    DEBUG_PRINT(("==========================")) 
    DEBUG_PRINT(("** CONTROL")) 

    char * ctrl = tostring(s->control,false);
    DEBUG_PRINT((ctrl)) 
    free(ctrl);
    DEBUG_PRINT(("** STORES : %d",s->free_adr)) 
    for(int i = 0; i < s->free_adr; i++){
        char * str =  tostring(s->storage[i],false);
        DEBUG_PRINT(("%d == %s",i,str))
        free(str);
    }
    DEBUG_PRINT(("** ENVIRONMENT : %d",s->environment->size)) 
    struct envnode *node = s->environment->bucket;
    while(node){
        DEBUG_PRINT(("%s at %d ",node->key,node->value))
        node = node->next;
    }
    DEBUG_PRINT(("** CONTINUATION")) 
    Value cc = MakeContinuation(s->continuation);
    DEBUG_PRINT((" --> cont type %s",tostring(cc,false)))
    DEBUG_PRINT(("==========================")) 
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    isatom
 *  Description:    simple check
 * =====================================================================================
 */
static bool isatom(Value el)
{
    enum Tag atoms[] = {LAM,INT,IS,SYMBOL,BOOLEAN,PRIM,VOID,LIST,QUOTE,CLOSURE};
    if(el.tt == VOID||el.tt == UNDEF) return true;
    for(int i = 0; i < NELEMS(atoms); i++){ 
        if(el.b->t == atoms[i]){return true;}
    }
    return false;
}
    

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    evalatom
 *  Description:    evaluate atomic expressions 
 *  Todo       :    implement primitives
 * =====================================================================================
 */
static Value evalatom(Value atom,environ * htbl,memory mem){

    switch(atom.tt){

        case VOID :
        case UNDEF :
            return atom;
    }

    switch(atom.b->t){

    case SYMBOL :{
        int adress = (int) get(htbl,(const char *) atom.s->name); 
        if(adress == -1){ 
            DEBUG_PRINT(("Storage failure for %s",atom.s->name))
            DEBUG_PRINT(("ENVIRONMENT : %d",htbl->size)) 
            struct envnode *node = htbl->bucket;
            while(node){
                DEBUG_PRINT(("%s at %d ",node->key,node->value))
                node = node->next;
            }
            exit(1);
        }
        //freevalue(atom);
        Value res = mem[adress]; 
        if(res.tt == UNDEF) {
            DEBUG_PRINT(("Unintialized Binding to %s",atom.s->name))
            exit(1);
        }
        return res;
    }

    case BOOLEAN :
    case INT : 
    case LIST : 
    case CLOSURE:
        return atom;
        break;

    case PRIM :{
        Value * parsed =  (Value *) malloc(atom.p->nargs * sizeof(Value));
        for(int i = 0; i < atom.p->nargs; i++){
            parsed[i] = evalatom(atom.p->arguments[i],htbl,mem);
        }
        Value sum = atom.p->exec(parsed[0],parsed[1]);
        for(int i = 2; i < atom.p->nargs; i++){
            sum = atom.p->exec(sum,parsed[i]); 
        }
        free(parsed);
        //freevalue(atom);
        return sum; 
    }

    case IS : {
        DEBUG_PRINT(("@Jumping to Secure"))

		// make Continue continuation
        kont kk; 
        kk.c    = malloc(sizeof(struct cont_kont));
        kk.c->t = KCONTINUE;
        kk.c->next = mystate->continuation;
        kk.c->e = copyenv(mystate->environment);
        mystate->continuation = kk;
		
        Value val;

        val.b = secure_eval(atom.i->label);  

        if(val.b == -1){
            DEBUG_PRINT(("Invalid Label"))
            exit(1);
        }
        
        return evalatom(val,mystate->environment,mystate->storage); 
    }

    case LAM :{
        Value cpy = copyvalue(atom); 
	    return MakeClosure(cpy,htbl);
    }

    case QUOTE :
        return atom.q->arg; 
        break;

    default :{ 
        char * tmp = tostring(atom,false);
        DEBUG_PRINT(("Unkown atom : %s",tmp))
        free(tmp);
        exit(1);
    } 
   }
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    applykont
 *  Description:    execute a continuation 
 * =====================================================================================
 */
static limbo applykont(Value val,kont k,state * s)
{
    if(k.empty == NULL){
        limbo ret = {NULL,{val,s}};
        return ret;
    }

    switch(k.l->t){

        case KLET : { // KOONT
            struct let_kont * lk = k.l; 
            insert(lk->e,lk->var.s->name,s->free_adr);
		    s->storage[s->free_adr] = copyvalue(val); // MEM : Don't clear  
		    s->free_adr++;
            //sfreevalue(&s->control);  
		    s->control      = lk->expr;
		    s->environment  = lk->e;
		    s->continuation = lk->next;
            limbo ret = {s,{}};
            return ret;
        }

        case KRET :{
            s->continuation = k.r->next;     
            s->control = val;
            limbo ret = {NULL,{val,s}};
            return ret;
        }

        case KCONTINUE : {
            s->control      = val;
            s->continuation = k.c->next;
            s->environment  = k.c->e;
            limbo ret       = {s,{}};
            return ret;
        }

        default :
            DEBUG_PRINT (("Unsupported Continuation"))
            exit(1);
    }
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    applyproc
 *  Description:    apply procedure to value
 * =====================================================================================
 */
static limbo applyproc(Value proc,Value * args,state *s){
    DEBUG_PRINT(("CALL PROCEDURE %s",tostring(proc,false)))
	if(proc.c->t == CLOSURE){

        int curr = s->free_adr;
        int nargs = proc.c->lambda.l->nargs; 
        DEBUG_PRINT(("Proc arg == %d",nargs))
        s->free_adr = curr + nargs; 

		// update enviroment with new adresses for each variable of lambda
        for(int j = curr,i = 0; j < s->free_adr ;j++){
            insert(proc.c->env,proc.c->lambda.l->arguments[i].s->name,j); 
            i++;
        }

		// update storage with adresses pointing to arguments
        for(int j = curr,i = 0; j < s->free_adr ;j++){
            DEBUG_PRINT(("Proc %d == %s",i,tostring((args[i]),false)))
            s->storage[j] = copyvalue(args[i]); // MEM : Don't clear
            i++;
        }
        
		// create new state with updated storage and envorinment, control = body of lambda
        s->environment = copyenv(proc.c->env);
        Value cpy = copyvalue(proc.c->lambda.l->body);
        //freevalue(&s->control);
        s->control = cpy; 
        limbo ret = {s,{}};
        return ret;
	}
	else if(proc.k->t == CONTINUATION){
    // TODO mem ?
        DEBUG_PRINT((">>>>> DOING CONTINUATION"))
		return applykont(args[0],proc.k->kstar, s);
	}
	else{
		DEBUG_PRINT(("Unkown Procedure %s",tostring(proc,false)))
        exit(1);
	}
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    step
 *  Description:    execute a step in the CESK machine 
 *  Todo       :	Done ? 
 * =====================================================================================
 */
static limbo step(state * s)
{
    // atom case
    if(isatom(s->control))
    {
        Value return_val = evalatom(s->control,s->environment,s->storage);
        return  applykont(return_val,s->continuation,s); 
    }

    switch(s->control.b->t){

    case IF :{
        Value condi = evalatom(s->control.f->cond,s->environment,s->storage);	

		if(condi.b->value == 1){
            Value cpy = copyvalue(s->control.f->cons);
            //freevalue(&s->control);
			s->control = cpy;
		}else{
            Value cpy = copyvalue(s->control.f->alt);
            //freevalue(&s->control);
			s->control = cpy;	
		}

        limbo ret = {s,{}};
		return ret;
	}


    case APPLICATION :{
        DEBUG_PRINT(("Appl arg == %d",s->control.a->nargs))
        Value * argum = (Value *) malloc(sizeof(Value) * s->control.a->nargs);
        for(int i = 0; i < s->control.a->nargs; i++){
            argum[i] = evalatom(s->control.a->arguments[i],s->environment,s->storage);
        }
        limbo res = applyproc(argum[0],(++argum),s);
        free(--argum);
        return res;
    }

    case CALLCC :{
        Value proc = evalatom(s->control.cc->function,s->environment,s->storage);
        Value curr = MakeContinuation(s->continuation);
        limbo res = applyproc(proc,&curr,s);
        return res;
    }

    case SET : {
        Value val = evalatom(s->control.sv->value,s->environment,s->storage);
        int adress = (int) get(s->environment,(const char *) s->control.sv->var.s->name); 
        FREECELL((s->storage[adress]))
        s->storage[adress] = copyvalue(val); // MEM 
        Value empty = {0};
        return applykont(empty,s->continuation,s);
    }

    case DEFINE : {
        if( s->control.d->var.s->t != SYMBOL) {DEBUG_PRINT(("Expected Symbol !!")) exit(1);}
        int test = (int) get(s->environment,(const char *) s->control.d->var.s->name); 
        if(test == -1){ 
            insert(s->environment,(const char *)s->control.d->var.s->name,s->free_adr++);
        }        
        
        // once binding exists preform set
        Value val = evalatom(s->control.d->expr,s->environment,s->storage);
        int adress = (int) get(s->environment,(const char *) s->control.d->var.s->name); 
        FREECELL((s->storage[adress]))
        s->storage[adress] = copyvalue(val); // MEM
        Value empty = {0};
        return applykont(empty,s->continuation,s);
    }

    case LET : {
        Value a = copyvalue(s->control.lt->expr);
        Value b = copyvalue(s->control.lt->var);
        Value c = copyvalue(s->control.lt->body);
        //freevalue(&(s->control));
        s->control = a;

        // TODO continuation memory management
        // Make a letk with b,c and environment and previous cont
        struct let_kont * nn = malloc(sizeof(struct let_kont));
        nn->var  = b;
        nn->expr = c;
        nn->e    = s->environment; 
        nn->next = s->continuation;
        nn->t   = KLET;
        kont new;
        new.l = nn;
        s->continuation = new;
        limbo ret = {s,{}};
		return ret;
    }

    case LETREC : {
        int curr = s->free_adr;
        int nargs = s->control.lr->nargs; 
        s->free_adr = curr + nargs; 
		// update enviroment with new adresses for each variable of lambda
        for(int j = curr,i = 0; j < s->free_adr ;j++){
            insert(s->environment,(const char *)s->control.lr->vars[i].s->name,j); 
            i++;
        }
        Value * list = (Value*) malloc(s->control.lr->nargs * (sizeof(Value)));
        for(int i = 0; i < s->control.lr->nargs ;i++){
            // curry ?
            list[i] = evalatom(s->control.lr->exprs[i],s->environment,s->storage);
        }
		// update storage with adresses pointing to arguments
        for(int j = curr,i = 0; j < s->free_adr ;j++){
            s->storage[j] = copyvalue(list[i]); // MEM : new adress do not delete
            i++;
        }
        Value b = copyvalue(s->control.lr->body);
        //freevalue(&(s->control));
        s->control = b;
        limbo ret = {s,{}};
		return ret;
    } 

    case BEGIN : {
        Value proc;
        for(int i = 0; i < s->control.bg->nargs; i++){ 
            proc = evalatom(s->control.bg->stmts[i],
                s->environment,s->storage);
        }
        return  applykont(proc,s->continuation,s); 
    }

    case CAR : {
        struct Car * here = s->control.car;
        Value val = evalatom(here->arg,s->environment,s->storage);
        if(val.ls->t == LIST){
            if (val.ls->nargs > 0){
                return applykont(val.ls->args[0],s->continuation,s); 
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

    case CDR : {
        struct Cdr * here = s->control.cdr;
        Value val = evalatom(here->arg,s->environment,s->storage);
        if(val.ls->t == LIST){
            if (val.ls->nargs > 1){
                Value * newlist = (Value *) malloc((val.ls->nargs-1) * (sizeof (Value))); 
                for(int i = 1; i < val.ls->nargs; i++){
                    newlist[(i-1)] = copyvalue(val.ls->args[i]); 
                }
                free(val.ls->args);
                val.ls->args = newlist;
                val.ls->nargs--;
                return applykont(val,s->continuation,s); 
            }else if(val.ls->nargs == 1) {
                return applykont(MakeNIL(),s->continuation,s);
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
    case CONS : {
        Value v = evalatom(s->control.cons->arg,s->environment,s->storage);
        Value v2 = evalatom(s->control.cons->arg2,s->environment,s->storage);
        if(v2.ls->t == LIST && v2.ls->islist == true){
            Value * newlist = (Value *) malloc((v2.ls->nargs+1) * (sizeof (Value)));  
            newlist[0] = v;
            for(int i = 0; i < v2.ls->nargs; i++){
                newlist[(i+1)] = copyvalue(v2.ls->args[i]); 
            }
            free(v2.ls->args);
            v2.ls->args = newlist;
            v2.ls->nargs++;
            return applykont(v2,s->continuation,s);   
        }
        return applykont(MakePair(v,v2),s->continuation,s);   
    }

    case PAIRQ : {
        Value v = evalatom(s->control.pq->arg,s->environment,s->storage);  
        if(v.ls->t == LIST){
            if(v.ls->islist == true){
                return applykont(MakeBoolean((v.ls->nargs > 0)),s->continuation,s);  
            }
            return applykont(MakeBoolean((v.ls->nargs > 1)),s->continuation,s);  
        }
        else{
            DEBUG_PRINT(("Expected List"))
            exit(1);
        }
    }

    case LISTQ : {
        Value v = evalatom(s->control.lq->arg,s->environment,s->storage);  
        if(v.ls->t == LIST){
            return applykont(MakeBoolean(v.ls->islist),s->continuation,s); 
        }
        else{
            DEBUG_PRINT(("Expected List"))
            exit(1);
        }
    }   
    
    case NULLQ : {
        Value v = evalatom(s->control.nq->arg,s->environment,s->storage);  
        if(v.ls->t == LIST){
            return applykont(MakeBoolean(v.ls->nargs == 0),s->continuation,s); 
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

}}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    steprec
 *  Description:    step through program
 * =====================================================================================
 */
static answer steprec (state * s){
    static int i = 0;
    DEBUG_PRINT(("STEP# == %d of Insecure",++i))
    debugstate(s);
    limbo result = step(s); 
    if(result.computation == NULL){
        //DEBUG_PRINT(("DONE"))
        return result.ans;
    }
    else{
       return steprec(result.computation);
    }
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    inject
 *  Description:    create a new start state
 * =====================================================================================
 */
static void inject (void){

    static environ tbl = {NULL,0};
    static Value empty = {NULL};

    // create empty environment -- tested
    environ * envtable = (environ *) malloc(sizeof(environ));  
    *envtable = tbl;

    // inject state
    mystate = malloc(sizeof(state));
    mystate->control  = empty;
    mystate->environment = envtable;
    mystate->storage = (Value *) (malloc(MEM_SIZE * sizeof(Value))); 
    mystate->continuation.empty = NULL;
	mystate->free_adr = 0;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    evaluate
 *  Description:    main entry point for the outside
 *					argument is a pointer to deal with Sancus restriction
 * =====================================================================================
 */
extern Value evaluate(Value v){ 

	// make Return continuation
    kont kk;
    kk.r                  = malloc(sizeof(struct ret_kont));
    kk.r->t               = KRET;
    kk.r->next            = mystate->continuation;
    mystate->continuation = kk;

    mystate->control = v;
    answer ans  = steprec(mystate);
    DEBUG_PRINT(("END OF INSECURE STEP"))
	return ans.ans;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    run
 *  Description:    run the program 
 * =====================================================================================
 */
static void run (Value * program,int c){

    mystate->control = program[0];
    DEBUG_PRINT(("State has been injected")) 

    for(int i = 0; i < c ; i++){
        answer ans = steprec(mystate); 
        if(ans.ans.tt != 0) {
            char * result = tostring(ans.ans,true);
            printf("%s\n",result);
            // TODO clean up memory
            free(result);
        }
        mystate->control = program[(i+1)%c]; 
    }
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    main
 *  Description:    the startpoint
 *  Todo       :    implement conversion of arguments, preferably through file reads
 * =====================================================================================
 */
int main(int argc, char * argv[]){

    DEBUG_PRINT(("Taking input")) 
    inject();
    run(getinput(),getinput_n());
    return 0; 
}
