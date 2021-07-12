/**
 *    @file
 *    @brief This Function encodes a grayscale image into a JPEG2000 code stream
 *    specified in the JPEG2000 Part-1 standard.
 *    @author Gilbert ORG: W/NP11 @date 2002-12-02
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "jasper/jasper.h"
#define JAS_1_700_2

#ifdef __64BIT__
  typedef int g2int;
#else
  typedef long g2int;
#endif

#if defined CRAY90
   #include <fortran.h>
   #define SUB_NAME ENC_JPEG2000
#elif defined LINUXF90
   #define SUB_NAME ENC_JPEG2000
#elif defined LINUXG95
   #define SUB_NAME enc_jpeg2000__
#elif defined HP || defined AIX
   #define SUB_NAME enc_jpeg2000
#elif defined SGI || defined LINUX || defined VPP5000 || defined APPLE
   #define SUB_NAME enc_jpeg2000_
#endif

int SUB_NAME(unsigned char *cin,g2int *pwidth,g2int *pheight,g2int *pnbits,
                 g2int *ltype, g2int *ratio, g2int *retry, char *outjpc, 
                 g2int *jpclen)
/**
 *    Using JasPer Software version 1.500.4 (or 1.700.2 ) written by the 
 *    University of British Columbia, Image Power Inc, and others.
 *    JasPer is available at http: *   www.ece.uvic.ca/~mdadams/jasper/.
 *
 *    PROGRAM HISTORY LOG:
 *     2002-12-02  Gilbert
 *     2004-07-20  GIlbert - Added retry argument/option to allow option of
 *                           increasing the maximum number of guard bits to the
 *                           JPEG2000 algorithm.
 *
 *    USAGE:    int enc_jpeg2000(unsigned char *cin,g2int *pwidth,g2int *pheight,
 *                                g2int *pnbits, g2int *ltype, g2int *ratio, 
 *                                g2int *retry, char *outjpc, g2int *jpclen)
 *
 *    INPUT ARGUMENTS:
 *          cin   - Packed matrix of Grayscale image values to encode.
 *        pwidth  - Pointer to width of image
 *        pheight - Pointer to height of image
 *        pnbits  - Pointer to depth (in bits) of image.  i.e number of bits
 *                  used to hold each data value
 *        ltype   - Pointer to indicator of lossless or lossy compression
 *                  = 1, for lossy compression
 *                  != 1, for lossless compression
 *        ratio   - Pointer to target compression ratio.  (ratio:1)
 *                  Used only when *ltype == 1.
 *        retry   - Pointer to option type.
 *                  1 = try increasing number of guard bits
 *                  otherwise, no additional options
 *        jpclen  - Number of bytes allocated for new JPEG2000 code stream in
 *                  outjpc.
 *
 *     INPUT ARGUMENTS:
 *         outjpc - Output encoded JPEG2000 code stream
 *
 *     RETURN VALUES :
 *           0 = Length in bytes of encoded JPEG2000 code stream
 *             -3 = Error decode jpeg2000 code stream.
 *             -5 = decoded image had multiple color components.
 *                  Only grayscale is expected.
 *
 *     REMARKS:
 *
 *          Requires JasPer Software version 1.500.4 or 1.700.2
 *
 *     ATTRIBUTES:
 *       LANGUAGE: C
 *       MACHINE:  IBM SP
 */

{
    int ier,rwcnt;
    jas_image_t image;
    jas_stream_t *jpcstream,*istream;
    jas_image_cmpt_t cmpt,*pcmpt;
#define MAXOPTSSIZE 1024
    char opts[MAXOPTSSIZE];

    g2int width,height,nbits;
    width=*pwidth;
    height=*pheight;
    nbits=*pnbits;
/*
    printf(" enc_jpeg2000:width %ld\n",width);
    printf(" enc_jpeg2000:height %ld\n",height);
    printf(" enc_jpeg2000:nbits %ld\n",nbits);
    printf(" enc_jpeg2000:jpclen %ld\n",*jpclen);
*/
/*       jas_init(); */
 *   
 *     Set lossy compression options, if requested.
 *   
    if ( *ltype != 1 ) {
       opts[0]=(char)0;
    }
    else {
       snprintf(opts,MAXOPTSSIZE,"mode=real\nrate=%f",1.0/(float)*ratio);
    }
    if ( *retry == 1 ) {              *    option to increase number of guard bits
       strcat(opts,"\nnumgbits=4");
    }
/*    printf("SAGopts: %s\n",opts); */
    
 *   
 *     Initialize the JasPer image structure describing the grayscale
 *     image to encode into the JPEG2000 code stream.
 *   
    image.tlx_=0;
    image.tly_=0;
#ifdef JAS_1_500_4 
    image.brx_=(uint_fast32_t)width;
    image.bry_=(uint_fast32_t)height;
#endif 
#ifdef JAS_1_700_2
    image.brx_=(jas_image_coord_t)width;
    image.bry_=(jas_image_coord_t)height;
#endif
    image.numcmpts_=1;
    image.maxcmpts_=1;
#ifdef JAS_1_500_4
/*
**  grayscale Image
*/
    image.colormodel_=JAS_IMAGE_CM_GRAY;
#endif
#ifdef JAS_1_700_2
/*
**  grayscale Image
*/
    image.clrspc_=JAS_CLRSPC_SGRAY;
    image.cmprof_=0; 
#endif
/*       image.inmem_=1; */

    cmpt.tlx_=0;
    cmpt.tly_=0;
    cmpt.hstep_=1;
    cmpt.vstep_=1;
#ifdef JAS_1_500_4
    cmpt.width_=(uint_fast32_t)width;
    cmpt.height_=(uint_fast32_t)height;
#endif
#ifdef JAS_1_700_2
    cmpt.width_=(jas_image_coord_t)width;
    cmpt.height_=(jas_image_coord_t)height;
    cmpt.type_=JAS_IMAGE_CT_COLOR(JAS_CLRSPC_CHANIND_GRAY_Y);
#endif
    cmpt.prec_=nbits;
    cmpt.sgnd_=0;
    cmpt.cps_=(nbits+7)/8;

    pcmpt=&cmpt;
    image.cmpts_=&pcmpt;

/*   
**       Open a JasPer stream containing the input grayscale values
*/   
    istream=jas_stream_memopen((char *)cin,height*width*cmpt.cps_);
    cmpt.stream_=istream;

/*   
**       Open an output stream that will contain the encoded jpeg2000
**       code stream.
*/   
    jpcstream=jas_stream_memopen(outjpc,(int)(*jpclen));

/*   
**        Encode image.
*/   
    ier=jpc_encode(&image,jpcstream,opts);
    if ( ier != 0 ) {
       printf(" jpc_encode return = %d \n",ier);
       return -3;
    }
/*   
**        Clean up JasPer work structures.
*/       
    rwcnt=jpcstream->rwcnt_;
    ier=jas_stream_close(istream);
    ier=jas_stream_close(jpcstream);
/*   
**         Return size of jpeg2000 code stream
*/   
    return (rwcnt);

}

