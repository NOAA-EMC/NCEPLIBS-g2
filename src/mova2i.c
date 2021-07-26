/**
 * @file
 * @brief mova2i Moves a bit string from a char*1 to int
 * @author Stephen Gilbert @date 2002-08-15
 */

/**
 * This Function copies a bit string from a Character*1 variable
 * to an integer variable. It is intended to replace the Fortran Intrinsic
 * Function ICHAR, which only supports 0 <= ICHAR(a) <= 127 on the
 * IBM SP. If "a" is greater than 127 in the collating sequence,
 * ICHAR(a) does not return the expected bit value.
 * This function can be used for all values 0 <= ICHAR(a) <= 255.
 * 
 * @param[in] a - Character*1 variable that holds the bitstring to extract
 * @return - > mova2i - Integer value of the bitstring in character a
 * 
 * @author Stephen Gilbert @date 2002-08-15
 * */

#ifdef CRAY90
   #include <fortran.h>
   int MOVA2I(unsigned char *a)
#endif
#ifdef HP
   int mova2i(unsigned char *a)
#endif
#ifdef SGI
   int mova2i_(unsigned char *a)
#endif
#ifdef LINUX
   int mova2i_(unsigned char *a)
#endif
#ifdef LINUXF90
   int MOVA2I(unsigned char *a)
#endif
#ifdef LINUXG95
   int mova2i__(unsigned char *a)
#endif
#ifdef APPLE
   int mova2i__(unsigned char *a)
#endif
#ifdef VPP5000
   int mova2i_(unsigned char *a)
#endif
#ifdef IBM4
   int mova2i(unsigned char *a)
#endif
#ifdef IBM8
   long long int mova2i(unsigned char *a)
#endif

{
    return (int)(*a);
}
