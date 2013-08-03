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

// c-lib
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "load.h"


/*-----------------------------------------------------------------------------
 * Macro's 
 *-----------------------------------------------------------------------------*/
#define SINGLE(T,TYPE)     case N(T) : {\
            VALUE c;\
            c.b = N(readCode)(strbuf);\
			return N(make##TYPE)(c.b);\
        }

#define DOUBLE(T,TYPE)  case N(T) : {\
            VALUE a;\
            VALUE b;\
            a.b = N(readCode)(strbuf);\
            b.b = N(readCode)(strbuf);\
			return N(make##TYPE)(a.b,b.b);\
        }

#define MULTIPLE(T,TYPE) case N(T) : {\
            char *end;\
            long c = strtol((*strbuf)[0], &end, 10);\
            if (!(end == (*strbuf)[0] || *end != '\0' || errno == ERANGE)){\
                (*strbuf)++;\
                VALUE * ls = MALLOC(c * sizeof(VALUE));\
                for(long i = 0; i < c; i++){\
                    ls[i].b = N(readCode)(strbuf);\
                }\
				return N(make##TYPE)(c,ls);\
            }else{\
                DEBUG_PRINT("ERROR: Expecting Argument Count");\
                exit(1);\
            }\
        }

#ifdef SECURE // hack
HOOK void * OTHERN(readCode)(char***);
#endif


/*-----------------------------------------------------------------------------
 * Local functions
 *-----------------------------------------------------------------------------*/
LOCAL char ** split(char * a_str, const char * a_delim,int *len);
LOCAL void freesplit(char ** a_str);
LOCAL void * readType(char***);


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
LOCAL char** split(char *str, const char *delimiter,int *len){
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
    //*array=NULL;
    *len=c;
    return ret;
}
	

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    read in byte code
 *  Description:    convert triple pointer to VALUE
 * =====================================================================================
 */
LOCAL void * readType(char *** strbuf){
    char *end;
    long id = strtol((*strbuf)[0], &end, 10); 
    if (!(end == (*strbuf)[0] || *end != '\0' || errno == ERANGE)){
        (*strbuf)++;
        switch(id){

            case N(TIGNORE) : 
                return N(makeTIgnore)();

            case N(TUNIT) :
                return N(makeTUnit)();
            
            case N(TBOOLEAN) :
                return N(makeTBoolean)();

            case N(TINT) :
                return N(makeTInt)();

            case N(TARROW) : {
				// C evaluation order is not defined !!!
                TYPE left,right;
                left.byte = readType(strbuf);  
                right.byte = readType(strbuf);
                return N(makeTArrow)(left.byte,right.byte);
            }

            default :
                DEBUG_PRINT("ERROR: Unkown Identifier :: %d",id);
                exit(1);
        }
    }
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    read in byte code
 *  Description:    convert triple pointer to VALUE
 * =====================================================================================
 */
FUNCTIONALITY void * N(readCode)(char *** strbuf){
    char *end;
    long id = strtol((*strbuf)[0], &end, 10); 
    if (!(end == (*strbuf)[0] || *end != '\0' || errno == ERANGE)){
        (*strbuf)++;
        switch(id){

            case N(ERROR) :
                DEBUG_PRINT("Error in input file !!");
                exit(1);
                
            case N(UNIT) : 
                return N(makeUnit)(); 

            case N(NOP) :
                return N(makeNop)(); 

            case N(INT) : {
                char *end;
                long temp = strtol((*strbuf)[0], &end, 10); 
                if (!(end == (*strbuf)[0] || *end != '\0' || errno == ERANGE)){
                    (*strbuf)++;
                    return N(makeInt)((int) temp);
                }else{
                    DEBUG_PRINT("ERROR: Missing Identifier @INT.");
                    exit(1);
                }
            }

            case N(BOOLEAN) : {
                char *end;
                long temp = strtol((*strbuf)[0], &end, 10); 
                if (!(end == (*strbuf)[0] || *end != '\0' || errno == ERANGE)){
                    (*strbuf)++;
				    return N(makeBoolean)((int)temp);
                }else{
                    DEBUG_PRINT("ERROR: Missing Identifier @ BOOLEAN.");
                    exit(1);
                }
            }

            case N(SYMBOL) : {
                char *end;
                long c = strtol((*strbuf)[0], &end, 10); 
                if (!(end == (*strbuf)[0] || *end != '\0' || errno == ERANGE)){
                    (*strbuf)++;
                    char * temp = MALLOC((c+1) * sizeof(char));    
                    temp[0] ='\0'; 
                    strcat(temp,(*strbuf)[0]);
                    (*strbuf)++;
					return N(makeSymbol)(temp);
                }else{
                    DEBUG_PRINT("ERROR: Expecting length");
                    exit(1);
                }
            }

            SINGLE(CALLCC,Callcc) 
            SINGLE(CAR,Car) 
            SINGLE(CDR,Cdr) 
            SINGLE(QUOTE,Quote) 
            SINGLE(PAIRQ,PairQ) 
            SINGLE(LISTQ,ListQ) 
            SINGLE(NULLQ,NullQ) 

            case N(IF) : {
                VALUE a;
                VALUE b; 
                VALUE c;
                a.b = N(readCode)(strbuf);
                b.b = N(readCode)(strbuf);
                c.b = N(readCode)(strbuf);
			    return N(makeIf)(a.b,b.b,c.b);
            }

            case N(LET) : {
                VALUE a;
                VALUE b; 
                VALUE c;
                a.b = N(readCode)(strbuf);
                b.b = N(readCode)(strbuf);
                c.b = N(readCode)(strbuf);
			    return N(makeLet)(a.b,b.b,c.b);
            }

            DOUBLE(SET,Set)
            DOUBLE(CONS,Cons)
            DOUBLE(DEFINE,Define)

            case N(LAM) : {
                char *end;
                long c = strtol((*strbuf)[0], &end, 10); 
                if (!(end == (*strbuf)[0] || *end != '\0' || errno == ERANGE)){
                    (*strbuf)++;
                    VALUE * ls = MALLOC(c * sizeof(VALUE)); 
                    VALUE body; 
                    body.b = N(readCode)(strbuf);
                    for(int i = 0; i < c; i++){
                        ls[i].b = N(readCode)(strbuf);
                    }
                    return N(makeLambda)(c,body.b,ls);
                }else{
                    DEBUG_PRINT("ERROR: Expecting Argument Count");
                    exit(1);
                }
            }

            MULTIPLE(APPLICATION,Application) 
            MULTIPLE(BEGIN,Begin) 

            case N(LIST) : {
                char *end;
                long c = strtol((*strbuf)[0], &end, 10); 
                if (!(end == (*strbuf)[0] || *end != '\0' || errno == ERANGE)){
                    (*strbuf)++;
                    switch(c){
                    
                        case -1 : {
                            VALUE a;
                            VALUE b; 
                            a.b = N(readCode)(strbuf);
                            b.b = N(readCode)(strbuf);
                            return N(makePair)(a.b,b.b);
                        }

                        case 0 : {
                            return N(makeNIL)();
                        }

                        default : {
                            VALUE * ls = MALLOC(c * sizeof(VALUE)); 
                            for(int i = 0; i < c; i++){
                                ls[i].b = N(readCode)(strbuf);
                            }
                            return N(makeList)(c,ls);
                        }

                    }
                }
                break; 
            }

            case N(LETREC) : {
                char *end;
                long c = strtol((*strbuf)[0], &end, 10); 
                if (!(end == (*strbuf)[0] || *end != '\0' || errno == ERANGE)){
                    (*strbuf)++;
                    VALUE * ls = MALLOC((2*c) * sizeof(VALUE)); 
                    VALUE body; 
                    body.b  = N(readCode)(strbuf);
                    for(int i = 0; i < 2*c; i++){
                        ls[i].b = N(readCode)(strbuf);
                    }
                    return N(makeLetrec)(c,body.b,ls);
                }else{
                    DEBUG_PRINT("ERROR: Expecting Argument Count");
                    exit(1);
                }
                break;
            }

            case N(PRIM) : {
                char *end;
                long c = strtol((*strbuf)[0], &end, 10); 
                if (!(end == (*strbuf)[0] || *end != '\0' || errno == ERANGE)){
                    (*strbuf)++;
                    char op = ((*strbuf)[0])[0];
                    N(PrimOp) ref;
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
                    VALUE * ls = MALLOC((2*c) * sizeof(VALUE)); 
                    for(int i = 0; i < c; i++){
                        ls[i].b = N(readCode)(strbuf);
                    }
                    return N(makePrim)(c,ref,ls);
                }else{
                    DEBUG_PRINT("ERROR: Expecting Argument Count");
                    exit(1);
                }
                break;
            }

            #ifdef SECURE 
            case SI : {
                TYPE t;  
                OTHERVALUE v; 
                t.byte = readType(strbuf);
                v.b    = OTHERN(readCode)(strbuf); // TODO leak::memory to otherside
                return makeSI(t.byte,v.b);
            }

            #else
            case IS : { 
                char *end;
                TYPE t;
                t.byte = readType(strbuf);
                long temp = strtol((*strbuf)[0], &end, 10); 
                if (!(end == (*strbuf)[0] || *end != '\0' || errno == ERANGE)){
                    (*strbuf)++;
                    return makeIS(t.byte,(int)temp);
                }else{
                    DEBUG_PRINT("ERROR: Missing Identifier @INT.");
                    exit(1);
                }
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
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    read in byte code
 *  Description:    convert to storage locations
 * =====================================================================================
 */
#ifdef SECURE
FUNCTIONALITY ANNOTATION ** N(readByteCode)(char * input,int * line_n){
#else
FUNCTIONALITY VALUE * N(readByteCode)(char * input,int * line_n){
#endif

	int ignore = 0;
	
	
    char ** list = split(input,"\n",&ignore); 
    char ** orig = list;

    char *end;
    long lang = strtol(list[0], &end, 10); 
	N(language) = (enum LANGUAGE) lang;
    if (!(end == list[0] || *end != '\0' || errno == ERANGE)){

        // TODO no better solution ?
        if(lang < SCHEME && lang > ML ){ 
		    DEBUG_PRINT("Language not supported yet :: %d",lang);
        }

        char *end2;
        long lines = strtol(list[1], &end2, 10); 
        if (!(end == list[1] || *end2 != '\0' || errno == ERANGE)){

            *line_n = (int) lines;
            list +=2;
		    DEBUG_PRINT("Program -- Expecting %d lines", *line_n);

			#ifdef SECURE
			ANNOTATION ** locations = MALLOC(*line_n * sizeof(ANNOTATION*));
			for(int i = 0; i < *line_n ; i++){
				locations[i]			= MALLOC(sizeof(ANNOTATION));	
				(*(locations[i])).t.b		= N(readCode)(&list);
				(*(locations[i])).ty.byte = readType(&list);
			}
			#else
		    VALUE * locations = MALLOC(*line_n * sizeof(VALUE));     
		    for(int i = 0; i < *line_n ; i++){
			    locations[i].b = N(readCode)(&list);
		    }
			#endif
            
			// clear data
            freesplit(orig);

		    return locations;
        }
    }
    
    DEBUG_PRINT("ERROR: couldn't parse file format.\n");
    exit(1);
    return NULL;
}

