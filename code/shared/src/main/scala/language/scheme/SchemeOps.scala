package scalaam.language.scheme

object SchemeOps {

  /** These are the unary operations that should be supported by Scheme lattices */
  object UnaryOperator extends Enumeration {
    val
    /* Check the type of a value */
    IsNull, IsBoolean, IsCons, IsPointer, IsChar, IsSymbol, IsString, IsInteger, IsReal, IsVector,
        /* Negate a value */
    Not, /* Unary arithmetic operations */
    Ceiling, Floor, Round, Random, Sqrt, /* Transcendental functions */
    Sin, ASin, Cos, ACos, Tan, ATan, Log, /* Length operations */
    VectorLength, StringLength, /* Conversions */
    NumberToString, SymbolToString, StringToSymbol,
    ExactToInexact, InexactToExact, CharacterToInteger = Value
  }
  type UnaryOperator = UnaryOperator.Value

  /** Binary operations that should be supported by lattices */
  object BinaryOperator extends Enumeration {
    val
    /* Arithmetic operations */
    Plus, Minus, Times, Div, Quotient, Modulo, Remainder, /* Arithmetic comparison */
    Lt, /* Equality checking */
    NumEq, /* number equality */
    Eq, /* physical equality */
    /* String operations */
    StringAppend, StringRef, StringLt = Value
  }
  type BinaryOperator = BinaryOperator.Value
}
