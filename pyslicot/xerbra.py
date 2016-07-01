def xerbla(srname, info):
    """
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*     Python Version : Kangwon Lee July 2016

    Parameters
    ----------
    srname : character * 6 The name of the routine which called XERBLA
    info    The position of the invalid parameter in the parameter list of the calling routine

*  Purpose
*  =======
*
*  XERBLA  is an error handler for the LAPACK routines.
*  It is called by an LAPACK routine if an input parameter has an
*  invalid value.  A message is printed and execution stops.
*
*  Installers may consider modifying the STOP statement in order to
*  call system-specific exception-handling facilities.

    Returns
    -------

    """

    raise TypeError(' ** On entry to %6s parameter number %2d had an illegal value' % (srname, info))
