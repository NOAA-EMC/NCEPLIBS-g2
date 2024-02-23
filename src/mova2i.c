/**
 * @file
 * @brief Move bits from a char*1 to an int.
 * @author Stephen Gilbert @date 2002-08-15
 */

/**
 * Derefrence char pointer and cast result as 32-bit int.
 *
 * This function is intended to replace the Fortran Intrinsic Function
 * ICHAR, which only supports 0 <= ICHAR(a) <= 127 on the IBM SP. If
 * "a" is greater than 127 in the collating sequence, ICHAR(a) does
 * not return the expected bit value.  This function can be used for
 * all values 0 <= ICHAR(a) <= 255.
 * 
 * @param a Pointer to char.
 *
 * @return 32-bit integer containing the value of the bits.
 * 
 * @author Stephen Gilbert @date 2002-08-15
 * */
int g2_mova2i_(unsigned char *a)
{
    return (int)(*a);
}

/**
 * Derefrence char pointer and cast result as 64-bit int.
 *
 * See mova2i_() for details.
 * 
 * @param a Pointer to char.
 *
 * @return 64-bit integer containing value of the bits.
 * 
 * @author Ed Hartnett @date Feb 7, 2024
 * */
int g2_mova2i8_(unsigned char *a)
{
    return (long long int)(*a);
}
