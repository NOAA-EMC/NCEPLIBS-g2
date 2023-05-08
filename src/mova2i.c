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

int mova2i_(unsigned char *a)
{
    return (int)(*a);
}
