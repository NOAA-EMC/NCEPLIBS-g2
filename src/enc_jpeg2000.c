/**
 * @file
 * @brief This Function encodes a grayscale image into a JPEG2000 code stream
 * specified in the JPEG2000 Part-1 standard.
 * @author Stephen Gilbert @date 2002-12-02
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "jasper/jasper.h"

#ifdef __64BIT__
typedef int g2int; /**< Integer type. */
#else
typedef long g2int; /**< Long Integer type. */
#endif

/**
 * This Function encodes a grayscale image into a JPEG2000 code stream
 * specified in the JPEG2000 Part-1 standard.
 *
 * It uses JasPer Software written by the University of British
 * Columbia, Image Power Inc, and others.  JasPer is available at
 * http: www.ece.uvic.ca/~mdadams/jasper/.
 *
 * PROGRAM HISTORY LOG:
 * - 2002-12-02  Stephen Gilbert
 * - 2004-07-20  Stephen Gilbert - Added retry argument/option to allow option of
 * increasing the maximum number of guard bits to the JPEG2000 algorithm.
 *
 * @param[in] cin Packed matrix of Grayscale image values to encode.
 * @param[in] pwidth Pointer to width of image
 * @param[in] pheight Pointer to height of image
 * @param[in] pnbits Pointer to depth (in bits) of image.  i.e number of bits
 * used to hold each data value
 * @param[in] ltype Pointer to indicator of lossless or lossy compression
 * - = 1, for lossy compression
 * - != 1, for lossless compression
 * @param[in] ratio Pointer to target compression ratio.  (ratio:1)
 * Used only when *ltype == 1.
 * @param[in] retry   - Pointer to option type.
 * 1 = try increasing number of guard bits otherwise, no additional options
 * @param[in] jpclen  - Number of bytes allocated for new JPEG2000 code stream in outjpc.
 * @param[in] outjpc - Output encoded JPEG2000 code stream
 * @return
 * - 0 Length in bytes of encoded JPEG2000 code stream.
 * - -3 Error decode jpeg2000 code stream.
 * - -5 decoded image had multiple color components.
 * Only grayscale is expected.
 * @var int MAXOPTSSIZE
 *
 * @note Requires JasPer Software version 1.500.4 or 1.700.2
 *
 * @author Stephen Gilbert @date 2002-12-02
 */
int
enc_jpeg2000_(unsigned char *cin, g2int *pwidth, g2int *pheight, g2int *pnbits,
              g2int *ltype, g2int *ratio, g2int *retry, char *outjpc,
              g2int *jpclen)
{
    int ier,rwcnt;
    jas_image_t image;
    jas_stream_t *jpcstream,*istream;
    jas_image_cmpt_t cmpt,*pcmpt;
/**
 * \def MAXOPTSSIZE
 * Maximum size of the options.
 */
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

/*
**     Set lossy compression options, if requested.
*/
    if ( *ltype != 1 ) {
        opts[0]=(char)0;
    }
    else {
        snprintf(opts,MAXOPTSSIZE,"mode=real\nrate=%f",1.0/(float)*ratio);
    }
    if ( *retry == 1 ) {             // option to increase number of guard bits
        strcat(opts,"\nnumgbits=4");
    }
/*    printf("SAGopts: %s\n",opts); */

/*
**     Initialize the JasPer image structure describing the grayscale
**     image to encode into the JPEG2000 code stream.
*/
    image.tlx_=0;
    image.tly_=0;
    image.brx_=(jas_image_coord_t)width;
    image.bry_=(jas_image_coord_t)height;
    image.numcmpts_=1;
    image.maxcmpts_=1;
/*
**  grayscale Image
*/
    image.clrspc_=JAS_CLRSPC_SGRAY;
    image.cmprof_=0;
/*       image.inmem_=1; */

    cmpt.tlx_=0;
    cmpt.tly_=0;
    cmpt.hstep_=1;
    cmpt.vstep_=1;
    cmpt.width_=(jas_image_coord_t)width;
    cmpt.height_=(jas_image_coord_t)height;
    cmpt.type_=JAS_IMAGE_CT_COLOR(JAS_CLRSPC_CHANIND_GRAY_Y);
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
