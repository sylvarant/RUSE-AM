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

#ifndef SECURE

#include <string.h>

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



#ifdef BYTE

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    read
 *  Description:    copy file to string
 * =====================================================================================
 */
LOCAL char* read(const char * filen)
{
    FILE *ptr_file;
    char buf[128];     
    char *ret = malloc(1); 
    int retsize = 1; 
    ret[0]='\0';
    buf[0]='\0';

    ptr_file = fopen(filen,"r");
    if (!ptr_file) {
        DEBUG_PRINT("FAILED to open file");
        exit(1);
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
    if(argc != 3){
        DEBUG_PRINT("Byte eating requires two files");
        exit(1);
    }
   
    int lines = 0;
    DEBUG_PRINT("Input files %s %s",argv[1],argv[2]);
    char * input1 = read(argv[1]);
    char * input2 = read(argv[2]);
      
    // TODO :: secure ? Load secure module
    sload(input2); 
    free(input2);
    
    // read in insecure program
    VALUE * cmds = N(readByteCode)(input1,&lines);
    free(input1);
    inject();
    run(cmds,lines);
    return 0; 
}

#else

extern unsigned char _tmp_input2_byte_scm[];
extern unsigned char _tmp_input1_byte_scm[];

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
    VALUE * cmds = N(readByteCode)(_tmp_input1_byte_scm,&lines);
    inject();
    run(cmds,lines);
    return 0; 
}

#endif
#endif



