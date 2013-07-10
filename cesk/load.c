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
            VALUE c = readCode(strbuf);\
			result = make##TYPE(c);\
            break;\
        }

#define DOUBLE(T,TYPE)  case N(T) : {\
            VALUE a = readCode(strbuf);\
            VALUE b = readCode(strbuf);\
			result = make##TYPE(a,b);\
            break;\
        }

#define MULTIPLE(T,TYPE) case N(T) : {\
            int c = -1;\
            if(sscanf((*strbuf)[0], "%d\n",&c) == 1){\
                (*strbuf)++;\
                VALUE * ls = calloc(c,sizeof(VALUE));\
                for(int i = 0; i < c; i++){\
                    ls[i] = readCode(strbuf);\
                }\
				result = make##TYPE(c,ls);\
            }else{\
                DEBUG_PRINT("ERROR: Expecting Argument Count");\
                exit(1);\
            }\
            break;\
        }

#ifdef SECURE // hack
FUNCTIONALITY VALUE OTHERN(readCode)(const char*);
#endif


/*-----------------------------------------------------------------------------
 * Local functions
 *-----------------------------------------------------------------------------*/
LOCAL char ** str_split(char * a_str, const char a_delim);


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    str_split
 *  Description:    split a string across delimiters
 * =====================================================================================
 */
LOCAL char** str_split(char * a_str, const char a_delim)
{
    char** result    = 0;
    size_t count     = 0;
    char* tmp        = a_str;
    char* last_comma = 0;

    /* Count how many elements will be extracted. */
    while (*tmp)
    {
        if (a_delim == *tmp)
        {
            count++;
            last_comma = tmp;
        }
        tmp++;
    }

    /* Add space for trailing token. */
    count += last_comma < (a_str + strlen(a_str) - 1);

    /* Add space for terminating null string so caller
       knows where the list of returned strings ends. */
    count++;

    result = MALLOC(sizeof(char*) * count); 

    if (result)
    {
        size_t idx  = 0;
        char* token = strtok(a_str, ",");

        while (token)
        {
            *(result + idx++) = strdup(token);
            token = strtok(0, ",");
        }
        *(result + idx) = 0;
    }

    return result;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    read in byte code
 *  Description:    convert triple pointer to VALUE
 * =====================================================================================
 */
FUNCTIONALITY VALUE readCode(char *** strbuf){
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
            if(sscanf((*strbuf)[0], "%d\n",&temp) == 1){
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
            if(sscanf((*strbuf)[0],"%d\n",&temp) == 1){ 
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
                if(sscanf((*strbuf)[0], "%s\n",temp) == 1){ 
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
            VALUE a = readCode(strbuf);
            VALUE b = readCode(strbuf);
            VALUE c = readCode(strbuf);
			result  = N(makeIf)(a,b,c);
            break;
        }

        case N(LET) : {
            VALUE a = readCode(strbuf);
            VALUE b = readCode(strbuf);
            VALUE c = readCode(strbuf);
			result = N(makeLet)(a,b,c);
            break;
        }

        DOUBLE(SET,Set)
        DOUBLE(CONS,Cons)
        DOUBLE(DEFINE,Define)

        case N(LAM) : {
            int c = -1;    
            if(sscanf((*strbuf)[0], "%d\n",&c) == 1){ 
                (*strbuf)++;
                VALUE * ls = calloc(c,sizeof(VALUE)); 
                VALUE body = readCode(strbuf);
                for(int i = 0; i < c; i++){
                    ls[i] = readCode(strbuf);
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
        MULTIPLE(LIST,List) 

        case N(LETREC) : {
            int c = -1;    
            if(sscanf((*strbuf)[0],"%d\n",&c) == 1){ 
                (*strbuf)++;
                VALUE * ls = calloc(2*c,sizeof(VALUE)); 
                VALUE body = readCode(strbuf);
                for(int i = 0; i < 2*c; i++){
                    ls[i] = readCode(strbuf);
                }
                N(makeLetrec)(c,body,ls);
            }else{
                 DEBUG_PRINT("ERROR: Expecting Argument Count");
                exit(1);
            }
            break;
        }

        case N(PRIM) : {
            int c = -1;    
            if(sscanf((*strbuf)[0], "%d\n",&c) == 1){ 
                (*strbuf)++;
                char op = -1;
                N(PrimOp) ref;
                if(sscanf((*strbuf)[0],"%c\n",&op) == 1){ 
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
                    ls[i] = readCode(strbuf);
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
            result       = N(makeSI)(v);
            break;
        }

        #else
        case IS : { 
            int temp = -1;    
            if(sscanf((*strbuf)[0], "%d\n",&temp) == 1){
                (*strbuf)++;
                result = N(makeIS)(temp);
            }else{
                DEBUG_PRINT("ERROR: Missing Identifier @INT.");
                exit(1);
            }
            break;
        }

        #endif
       default :
        DEBUG_PRINT("ERROR: Unkown Identifier :: %d.\n",id);
        exit(1);
    }
    }else{
        DEBUG_PRINT("ERROR: Missing Identifier @ fscan.\n");
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
FUNCTIONALITY VALUE * N(readByteCode)(STATE * st,char * input,int * line_n){
    int lang;

    char ** list = str_split(input,'\n'); 
    char ** orig = list;
    
    if(sscanf(list[0],"%d",&lang) == 0){

        // TODO improve
        if(lang > 0){
		    DEBUG_PRINT("Language not supported yet :: %d",lang);
        }

        
        if(sscanf(list[1],"%d",line_n) == 0){

		    DEBUG_PRINT("Scheme Program -- Expecting %d lines",*line_n);

		    VALUE * locations = calloc(*line_n,sizeof(VALUE));     
            list +=2;
		    for(int i = 0; i < *line_n ; i++){
			    locations[i] = readCode(&list);
			    DEBUG_PRINT("line contents = %s",tostring(*(locations[i]),false));
		    }
            
            free(orig);
            #ifndef SECURE
		    return locations;
            #else
            for(int i = 0; i < *line_n; i++){
                DEBUG_PRINT("Adding Label :: %d",st->free_adr);
                st->storage[st->free_adr] = locations[i];
                insertLabel(&(st->label),st->free_adr);
                st->free_adr++;
            }
            return NULL;
            #endif
        }
    }
    
    DEBUG_PRINT("ERROR: couldn't parse file format.\n");
    exit(1);
    return NULL;
}

