/**
 * @file
 * @brief Header for JPEG C code.
 *
 * @author Ed Hartnett @date 12/23/22
 */

/** Long integer type. */
typedef int64_t g2int;

/** Unsigned long integer type. This typedef is provided for backward
 * compatibility and is not used by the library any more. */
typedef uint64_t g2intu;

/** Float type. This typedef is provided for backward compatibility
 * and is not used by the library any more. Use float in new code. */
typedef float g2float;

#define G2_JASPER_INIT -2         /**< In enc_jpeg2000()/dec_jpeg2000() error initializing jasper library. */
#define G2_JASPER_ENCODE -3       /**< In enc_jpeg2000() error encoding image with jasper. */
#define G2_JASPER_DECODE -3       /**< In dec_jpeg2000() error decoding image with jasper. */
#define G2_JASPER_DECODE_COLOR -5 /**< In dec_jpeg2000() decoded image had multiple color components. */

/** Name of JPEG codec in Jasper. */
#define G2C_JASPER_JPEG_FORMAT_NAME "jpc"

/** Max memory size setting for Jasper. */
#define G2C_JASPER_MAX_MEMORY 1073741824

