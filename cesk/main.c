/*
 * =====================================================================================
 *
 *       Filename:  main.c
 *
 *    Description:  starting point
 *
 *        Created:  07/09/2013 16:07:58
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

#include "global.h"
#include "cesk.h"
#include "load.h"
#include "string.h"


/*-----------------------------------------------------------------------------
 *  Local functions
 *-----------------------------------------------------------------------------*/
#ifdef BYTE
LOCAL char * read(const char * filen);
#endif



#ifndef SECURE

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    run
 *  Description:    run the program  
 * =====================================================================================
 */
HOOK void run (VALUE * program,int c){

    mystate->control = program[0];

    for(int i = 0; i < c ; i++){
        VALUE ans = steprec(); 
        if(ans.tt != N(NOP)){
            char * result = N(toString)(ans,1);
            printf("%s\n",result);
            free(result);
        }
        mystate->control = program[(i+1)%c]; 
    }
    free(program);
    free(mystate->storage);
    free(mystate);
}


#ifdef BYTE

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    read
 *  Description:    copy file to string
 * =====================================================================================
 */
LOCAL char* read(char * filen)
{
    FILE *ptr_file;
    char buf[128];     
    char *ret = malloc(1); 
    int retsize = 1; 
    ret[0]='\0';
    buf[0]='\0';

    ptr_file = fopen(FILENAME,"r");
    if (!ptr_file) {
        DEBUG_PRINT("FAILED to open file");
        exit 1;
    }
    while (fgets(buf,128, ptr_file)!=NULL)
    {
        retsize += strlen(buf)+1;  
        ret = realloc(ret, retsize); 
        strcat(ret, buf);          
    }
    fclose(ptr_file);
    return ret;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    main
 *  Description:    the startpoint for a machine without file reads, file is added
 *      to the binary
 * =====================================================================================
 */
int main(int argc,const char ** argv){
    if(argc != 2){
        DEBUG_PRINT("Byte eating requires two files");
        exit(1);
    }
   
    int lines = 0;
    char * input1 = read(argv[1]);
    char * input2 = read(argv[2]);
      
    // TODO :: secure ? Load secure module
    sload(input2); 
    
    // read in insecure program
    VALUE * cmds = N(readByteCode)(NULL,input1,&lines);
    inject();
    run(cmds,lines);
    return 0; 
}

#else

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    main
 *  Description:    the startpoint for a machine without file reads, file is added
 *      to the binary
 * =====================================================================================
 */
int main(void){

    #ifdef SANCUS_SPM
    WDTCTL = WDTPW | WDTHOLD;
    #endif

    sload(_tmp_input2_byte_scm);

    int lines =0;
    VALUE * cmds = N(readByteCode)(NULL,input1,&lines);
    inject();
    run(cmds,lines);
    return 0; 
}

#endif



