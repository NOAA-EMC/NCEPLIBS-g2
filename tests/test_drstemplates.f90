! This program tests the drstemplates module of the NCEPLIBS-g2
! project.
!
! Ed Hartnett 9/30/21
program test_drstemplates
  use drstemplates
  implicit none

  print *, 'Testing the drstemplates module.'

  print *, 'Testing getdrsindex...'
  
  print *, 'Testing getdrstemplate...'
  
  print *, 'Testing extdrstemplate...'
  
  print *, 'SUCCESS!'
  
end program test_drstemplates
