@mainpage

# Introduction

This document briefly describes the routines available for
encoding/decoding GRIB Edition 2 (GRIB2) messages with the NCEPLIBS-g2
library.

A GRIB Edition 2 message is a machine independent format for storing
one or more gridded data fields. Each GRIB2 message consists of the
following sections:

<pre>
SECTION 0 - Indicator Section
SECTION 1 - Identification Section
SECTION 2 - (Local Use Section) - optional                           }
SECTION 3 - Grid Definition Section                     }            }
SECTION 4 - Product Definition Section    }             }            }(repeated)
SECTION 5 - Data Representation Section   }             }(repeated)  }
SECTION 6 - Bit-map Section               }(repeated)   }            }
SECTION 7 - Data Section                  }             }            }
SECTION 8 - End Section                   }             }            }
</pre>

Sequences of GRIB sections 2 to 7, 3 to 7, or sections 4 to 7 may be
repeated within a single GRIB message. All sections within such
repeated sequences must be present and shall appear in the numerical
order noted above. Unrepeated sections remain in effect until
redefined.

For detailed information on GRIB2 see the [NCEP WMO GRIB2
Documentation](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/).

# GRIB2 Encoding Routines

Since a GRIB2 message can contain gridded fields for many parameters
on a number of different grids, several routines are used to encode a
message. This should give users more flexibility in how to organize
data within one or more GRIB2 messages.

To start a new GRIB2 message, call subroutine gribcreate(). It encodes
Sections 0 and 1 at the beginning of the message. This routine must be
used to create each message.

Subroutine addlocal() can be used to add a Local Use Section (Section
2). Note that section is optional and need not appear in a GRIB2
message.

Subroutine addgrid() is used to encode a grid definition into Section
3. This grid definition defines the geometry of the the data values in
the fields that follow it. addgrid() can be called again to change the
grid definition describing subsequent data fields.

Each data field is added to the GRIB2 message using routine
addfield(), which adds Sections 4, 5, 6, and 7 to the message.

After all desired data fields have been added to the GRIB2 message, a
call to routine gribend() is needed to add the final section 8 to the
message and to update the length of the message. A call to gribend()
is required for each GRIB2 message.

# GRIB2 Decoding Routines

Subroutine gb_info() can be used to find out how many Local Use
sections and data fields are contained in a given GRIB2 message. In
addition, this routine also returns the number of octets of the
largest Local Use section in the message. This value can be used to
ensure that the output array of subroutine getlocal() (described
below) is dimensioned large enough.

Subroutine getlocal() will return the requested occurrence of Section
2 from a given GRIB2 message.

gf_getfld() can be used to get all information pertaining to the nth
data field in the message. The subroutine returns all the unpacked
values for each Section and Template in a Fortran 90 derived type @ref
grib_mod::gribfield. An option exists that lets the user decide if the
subroutine should unpack the Bit-map (if applicable) and the data
values or just return the field description information.

Note that derived type @ref grib_mod::gribfield contains pointers to
dynamically allocated space that holds the contents of all arrays, and
users must free this memory with subroutine gf_free().

# Extracting GRIB2 Fields from a GRIB2 file

Subroutine getgb2() can be used to extract a specified field from a
file containing many GRIB2 messages. getgb2() searches an index to
find the location of the user specified field. The index can be
supplied from a seperate GRIB2 index file, or it can be generated
internally.

The GRIB2 file (and the index file, if supplied) must be opened with a
call to subroutine [baopen() or
baopenr()](https://noaa-emc.github.io/NCEPLIBS-bacio/) prior to the
call to getgb2().

The decoded information for the selected GRIB field is returned in a
derived type variable of type @ref grib_mod::gribfield. Users of this
routine will need to include the line 'use grib_mod' in their calling
routine.

Note that derived type @ref grib_mod::gribfield contains pointers to
many arrays of data. The memory for these arrays is allocated when the
values in the arrays are set. Users must free this memory, when it is
no longer needed, with gf_free().

Example usage:

@code
      use grib_mod
      type(gribfield) :: gfld
      integer,dimension(200) :: jids,jpdt,jgdt
      logical :: unpack=.true.
      ifile=10
  ! Open GRIB2 file
      call baopenr(ifile,"filename",iret)
      .
  ! Set GRIB2 field identification values to search for
      jdisc=
      jids(?)=
      jpdtn=
      jpdt(?)=
      jgdtn=
      jgdt(?)=

  ! Get field from file
      call getgb2(ifile,0,j,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,
     &            unpack,j,gfld,iret)

  ! Process field ...
      firstval=gfld%fld(1)
      lastval=gfld%fld(gfld%ndpts)
      fldmax=maxval(gfld%fld)
      fldmin=minval(gfld%fld)

  ! Free memory when done with field
      call gf_free(gfld)

      stop
      end
@endcode

# GRIB2 Tables/Templates

NCO Provides documentation on WMO GRIB2 at
https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/

WMO's GRIB2 specification "FM 92-XII GRIB - General
Regularly-distributed Information in Binary Form" contains
descriptions of each template and code table information. This
document can be found at https://codes.wmo.int/grib2/_codeflag (PDF
and MSWord formats are available).
