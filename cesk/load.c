/*
 * =====================================================================================
 *
 *       Filename:  load.c
 *
 *    Description:  Loading functionality
 *
 *        Created:  07/09/2013 16:06:55
 *
 *        Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */


#include <stdlib.h>
#include <stdio.h>

#include "cesk.h"

#include <string.h>

/*-----------------------------------------------------------------------------
 * Macro's 
 *-----------------------------------------------------------------------------*/
#define SINGLE(T,TYPE)     case N(T) : {\
            VALUE c = N(readCode)(strbuf);\
			result = N(make##TYPE)(c);\
            break;\
        }

#define DOUBLE(T,TYPE)  case N(T) : {\
            VALUE a = N(readCode)(strbuf);\
            VALUE b = N(readCode)(strbuf);\
			result = N(make##TYPE)(a,b);\
            break;\
        }

#define MULTIPLE(T,TYPE) case N(T) : {\
            int c = -1;\
            if(sscanf((*strbuf)[0], "%d",&c) == 1){\
                (*strbuf)++;\
                VALUE * ls = calloc(c,sizeof(VALUE));\
                for(int i = 0; i < c; i++){\
                    ls[i] = N(readCode)(strbuf);\
                }\
				result = N(make##TYPE)(c,ls);\
            }else{\
                DEBUG_PRINT("ERROR: Expecting Argument Count");\
                exit(1);\
            }\
            break;\
        }

#ifdef SECURE // hack
FUNCTIONALITY OTHERVALUE OTHERN(readCode)(char***);
#endif


/*-----------------------------------------------------------------------------
 * Local functions
 *-----------------------------------------------------------------------------*/
LOCAL char ** split(const char * a_str, const char * a_delim, size_t *len);
LOCAL void freesplit(char ** a_str);


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    freesplit
 *  Description:    free the result of splitting
 * =====================================================================================
 */
void freesplit(char** sa){
    char **array=sa;

    if(sa!=NULL){
        free(array[0]);//for text
        free(sa);      //for array
    }
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    str_split
 *  Description:    split a string across delimiters
 * =====================================================================================
 */
LOCAL char** split(const char *str, const char *delimiter, size_t *len){
    char *text, *p, *first, **array;
    int c;
    char** ret;

    *len = 0;
    text=strdup(str);//strdup not standard
    if(text==NULL) return NULL;
    for(c=0,p=text;NULL!=(p=strtok(p, delimiter));p=NULL, c++)//count item
        if(c==0) first=p; //first token top

    ret=(char**)malloc(sizeof(char*)*c+1);//+1 for NULL
    if(ret==NULL){
        free(text);
        return NULL;
    }
    //memmove?
    strcpy(text, str+(first-text));//skip until top token
    array=ret;
    for(p=text;NULL!=(p=strtok(p, delimiter));p=NULL){
        *array++=p;
    }
    *array=NULL;
    *len=c;
    return ret;
}
	

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    read in byte code
 *  Description:    convert triple pointer to VALUE
 * =====================================================================================
 */
FUNCTIONALITY VALUE N(readCode)(char *** strbuf){
    int id = -1;
	VALUE result = {0};
    if(sscanf((*strbuf)[0],"%d",&id) == 1){
    (*strbuf)++;
    switch(id){

        case N(ERROR) :
            DEBUG_PRINT("Error in input file !!");
            break;
                
        case N(VOID) : 
            result = N(makeVoid)(); 
            break;

        case N(NOP) :
            result = N(makeNop)(); 
            break;

        case N(INT) : {
            int temp = -1;    
            if(sscanf((*strbuf)[0], "%d",&temp) == 1){
                (*strbuf)++;
                result = N(makeInt)(temp);
            }else{
                DEBUG_PRINT("ERROR: Missing Identifier @INT.");
                exit(1);
            }
            break;
        }

        case N(BOOLEAN) : {
            int temp = -1;    
            if(sscanf((*strbuf)[0],"%d",&temp) == 1){ 
                (*strbuf)++;
				result = N(makeBoolean)(temp);
            }else{
                DEBUG_PRINT("ERROR: Missing Identifier @ BOOLEAN.");
                exit(1);
            }
            break;
        }

        case N(SYMBOL) : {
            int c = -1;    
            if(sscanf((*strbuf)[0], "%d",&c) == 1){ 
                (*strbuf)++;
                char * temp = calloc(c+1,sizeof(char));    
                if(sscanf((*strbuf)[0], "%s",temp) == 1){ 
                    (*strbuf)++;
					result = N(makeSymbol)(temp);
                }else{
                    DEBUG_PRINT("ERROR: Expecting String.");
                    exit(1);
                }
            }else{
                DEBUG_PRINT("ERROR: Expecting length");
                exit(1);
            }
            break;
        }

        SINGLE(CALLCC,Callcc) 
        SINGLE(CAR,Car) 
        SINGLE(CDR,Cdr) 
        SINGLE(QUOTE,Quote) 
        SINGLE(PAIRQ,PairQ) 
        SINGLE(LISTQ,ListQ) 
        SINGLE(NULLQ,NullQ) 

        case N(IF) : {
            VALUE a = N(readCode)(strbuf);
            VALUE b = N(readCode)(strbuf);
            VALUE c = N(readCode)(strbuf);
			result  = N(makeIf)(a,b,c);
            break;
        }

        case N(LET) : {
            VALUE a = N(readCode)(strbuf);
            VALUE b = N(readCode)(strbuf);
            VALUE c = N(readCode)(strbuf);
			result = N(makeLet)(a,b,c);
            break;
        }

        DOUBLE(SET,Set)
        DOUBLE(CONS,Cons)
        DOUBLE(DEFINE,Define)

        case N(LAM) : {
            int c = -1;    
            if(sscanf((*strbuf)[0], "%d",&c) == 1){ 
                (*strbuf)++;
                VALUE * ls = calloc(c,sizeof(VALUE)); 
                VALUE body = N(readCode)(strbuf);
                for(int i = 0; i < c; i++){
                    ls[i] = N(readCode)(strbuf);
                }
                result = N(makeLambda)(c,body,ls);
            }else{
                DEBUG_PRINT("ERROR: Expecting Argument Count");
                exit(1);
            }
            break;
        }

        MULTIPLE(APPLICATION,Application) 
        MULTIPLE(BEGIN,Begin) 

        case N(LIST) : {
            int c = -2;
            if(sscanf((*strbuf)[0],"%d",&c) == 1){ 
                (*strbuf)++;
                switch(c){
                    
                    case -1 : {
                        VALUE a = N(readCode)(strbuf);
                        VALUE b = N(readCode)(strbuf);
                        result = N(makePair)(a,b);
                        break;
                    }

                    case 0 : {
                        result = N(makeNIL)();
                        break;
                    }

                    default : {
                        VALUE * ls = calloc(c,sizeof(VALUE)); 
                        for(int i = 0; i < c; i++){
                            ls[i] = N(readCode)(strbuf);
                        }
                        result = N(makeList)(c,ls);
                    }

                }
            }
            break; 
        }

        case N(LETREC) : {
            int c = -1;    
            if(sscanf((*strbuf)[0],"%d",&c) == 1){ 
                (*strbuf)++;
                VALUE * ls = calloc(2*c,sizeof(VALUE)); 
                VALUE body = N(readCode)(strbuf);
                for(int i = 0; i < 2*c; i++){
                    ls[i] = N(readCode)(strbuf);
                }
                result = N(makeLetrec)(c,body,ls);
            }else{
                 DEBUG_PRINT("ERROR: Expecting Argument Count");
                exit(1);
            }
            break;
        }

        case N(PRIM) : {
            int c = -1;    
            if(sscanf((*strbuf)[0], "%d",&c) == 1){ 
                (*strbuf)++;
                char op = -1;
                N(PrimOp) ref;
                if(sscanf((*strbuf)[0],"%c",&op) == 1){ 
                    (*strbuf)++; 
                    switch(op){
                        case '+' : ref = N(sumPrim); break;
                        case '-' : ref = N(differencePrim); break;
                        case '=' : ref = N(numequalPrim); break;
                        case '*' : ref = N(productPrim); break;
                        default :
                            DEBUG_PRINT("ERROR: Could Not Match Operator");
                            exit(1);
                    }
                }else{
                    DEBUG_PRINT("ERROR: Expecting Argument Count");
                    exit(1);
                }
                VALUE * ls = calloc(c,sizeof(VALUE)); 
                for(int i = 0; i < c; i++){
                    ls[i] = N(readCode)(strbuf);
                }
                result = N(makePrim)(c,ref,ls);
            }else{
                DEBUG_PRINT("ERROR: Expecting Argument Count");
                exit(1);
            }
            break;
        }

        #ifdef SECURE 
        case SI : {
            OTHERVALUE v = OTHERN(readCode)(strbuf); // TODO leak :: passing out memory to the other side !!
            result       = makeSI(v);
            break;
        }

        #else
        case IS : { 
            int temp = -1;    
            if(sscanf((*strbuf)[0], "%d",&temp) == 1){
                (*strbuf)++;
                result = makeIS(temp);
            }else{
                DEBUG_PRINT("ERROR: Missing Identifier @INT.");
                exit(1);
            }
            break;
        }

        #endif
       default :
        DEBUG_PRINT("ERROR: Unkown Identifier :: %d",id);
        exit(1);
    }
    }else{
        DEBUG_PRINT("ERROR: Missing Identifier @ fscan");
        exit(1);
    }

    return result;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    read in byte code
 *  Description:    convert to storage locations
 * =====================================================================================
 */
FUNCTIONALITY VALUE * N(readByteCode)(char * input,int * line_n){

    int lang = -1;
	int ignore = 0;
	
	
    char ** list = split(input,"\n",&ignore); 
    char ** orig = list;
    
    if(sscanf(list[0],"%d",&lang) == 1){

        // TODO improve
        if(lang > 0){
		    DEBUG_PRINT("Language not supported yet :: %d",lang);
        }

        
        if(sscanf(list[1],"%d",line_n) == 1){

		    DEBUG_PRINT("Scheme Program -- Expecting %d lines",*line_n);

		    VALUE * locations = calloc(*line_n,sizeof(VALUE));     
            list +=2;
		    for(int i = 0; i < *line_n ; i++){
			    locations[i] = N(readCode)(&list);
		    }
            
			// clear data
            freesplit(orig);

		    return locations;
        }
    }
    
    DEBUG_PRINT("ERROR: couldn't parse file format.\n");
    exit(1);
    return NULL;
}

