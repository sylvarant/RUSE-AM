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
#include <string.h>
#include <errno.h>

#include "cesk.h"



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
            char *end;\
            long c = strtol((*strbuf)[0], &end, 10);\ 
            if (!(end == (*strbuf)[0] || *end != '\0' || errno == ERANGE)){\
                (*strbuf)++;\
                VALUE * ls = MALLOC(c * sizeof(VALUE));\
                for(long i = 0; i < c; i++){\
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
	VALUE result = {0};
    char *end;
    long id = strtol((*strbuf)[0], &end, 10); 
    if (!(end == (*strbuf)[0] || *end != '\0' || errno == ERANGE)){
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
                char *end;
                long temp = strtol((*strbuf)[0], &end, 10); 
                if (!(end == (*strbuf)[0] || *end != '\0' || errno == ERANGE)){
                    (*strbuf)++;
                    result = N(makeInt)((int) temp);
                }else{
                    DEBUG_PRINT("ERROR: Missing Identifier @INT.");
                    exit(1);
                }
                break;
            }

            case N(BOOLEAN) : {
                char *end;
                long temp = strtol((*strbuf)[0], &end, 10); 
                if (!(end == (*strbuf)[0] || *end != '\0' || errno == ERANGE)){
                    (*strbuf)++;
				    result = N(makeBoolean)((int)temp);
                }else{
                    DEBUG_PRINT("ERROR: Missing Identifier @ BOOLEAN.");
                    exit(1);
                }
                break;
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
					result = N(makeSymbol)(temp);
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
                char *end;
                long c = strtol((*strbuf)[0], &end, 10); 
                if (!(end == (*strbuf)[0] || *end != '\0' || errno == ERANGE)){
                    (*strbuf)++;
                    VALUE * ls = MALLOC(c * sizeof(VALUE)); 
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
                char *end;
                long c = strtol((*strbuf)[0], &end, 10); 
                if (!(end == (*strbuf)[0] || *end != '\0' || errno == ERANGE)){
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
                            VALUE * ls = MALLOC(c * sizeof(VALUE)); 
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
                char *end;
                long c = strtol((*strbuf)[0], &end, 10); 
                if (!(end == (*strbuf)[0] || *end != '\0' || errno == ERANGE)){
                    (*strbuf)++;
                    VALUE * ls = MALLOC((2*c) * sizeof(VALUE)); 
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
                char *end;
                long temp = strtol((*strbuf)[0], &end, 10); 
                if (!(end == (*strbuf)[0] || *end != '\0' || errno == ERANGE)){
                    (*strbuf)++;
                    result = makeIS((int)temp);
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

	int ignore = 0;
	
	
    char ** list = split(input,"\n",&ignore); 
    char ** orig = list;

    char *end;
    long lang = strtol(list[0], &end, 10); 
    if (!(end == list[0] || *end != '\0' || errno == ERANGE)){

        // TODO improve
        if(lang > 0){
		    DEBUG_PRINT("Language not supported yet :: %d",lang);
        }

        
        char *end2;
        long lines = strtol(list[1], &end2, 10); 
        if (!(end == list[1] || *end2 != '\0' || errno == ERANGE)){

            *line_n = (int) lines;
		    DEBUG_PRINT("Scheme Program -- Expecting %d lines",*line_n);
		    VALUE * locations = MALLOC(*line_n * sizeof(VALUE));     
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

