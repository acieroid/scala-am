package scalaam.core

/** An error that is not thrown but rather used as an erroneous value */
trait Error

/** An error that is thrown as an exception */
trait ScalaAMException extends Throwable
