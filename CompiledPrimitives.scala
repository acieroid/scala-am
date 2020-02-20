object `<=` extends StoreOperation("<=", Some(2)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    val `y_pos` = args(1)._1
    val `y` = args(1)._2
    `<`.call(fpos, (-1,-23), List((`x_pos`, `x`), (`y_pos`, `y`)), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        bool(true)
      }
      {
        `=`.call(fpos, (-1,-31), List((`x_pos`, `x`), (`y_pos`, `y`)), store, alloc).map(_._1)
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("<=", 2, args.length))
}

object `>=` extends StoreOperation(">=", Some(2)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    val `y_pos` = args(1)._1
    val `y` = args(1)._2
    `>`.call(fpos, (-1,-23), List((`x_pos`, `x`), (`y_pos`, `y`)), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        bool(true)
      }
      {
        `=`.call(fpos, (-1,-31), List((`x_pos`, `x`), (`y_pos`, `y`)), store, alloc).map(_._1)
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError(">=", 2, args.length))
}

object `>` extends StoreOperation(">", Some(2)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    val `y_pos` = args(1)._1
    val `y` = args(1)._2
    `<=`.call(fpos, (-1,-23), List((`x_pos`, `x`), (`y_pos`, `y`)), store, alloc).map(_._1) >>= { `_0`  =>
      `not`.call(fpos, (-1,-18), List(((-1,-23), `_0`)), store, alloc).map(_._1)
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError(">", 2, args.length))
}

object `zero?` extends StoreOperation("zero?", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `=`.call(fpos, (-1,-20), List((`x_pos`, `x`), ((0, 0), number(0))), store, alloc).map(_._1) }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("zero?", 1, args.length))
}

object `positive?` extends StoreOperation("positive?", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `>`.call(fpos, (-1,-24), List((`x_pos`, `x`), ((0, 0), number(0))), store, alloc).map(_._1) }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("positive?", 1, args.length))
}

object `negative?` extends StoreOperation("negative?", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `<`.call(fpos, (-1,-24), List((`x_pos`, `x`), ((0, 0), number(0))), store, alloc).map(_._1) }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("negative?", 1, args.length))
}

object `odd?` extends StoreOperation("odd?", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `modulo`.call(fpos, (-1,-24), List((`x_pos`, `x`), ((0, 0), number(2))), store, alloc).map(_._1) >>= { `_0`  =>
      `=`.call(fpos, (-1,-19), List(((0, 0), number(1)), ((-1,-24), `_0`)), store, alloc).map(_._1)
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("odd?", 1, args.length))
}

object `even?` extends StoreOperation("even?", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `modulo`.call(fpos, (-1,-25), List((`x_pos`, `x`), ((0, 0), number(2))), store, alloc).map(_._1) >>= { `_0`  =>
      `=`.call(fpos, (-1,-20), List(((0, 0), number(0)), ((-1,-25), `_0`)), store, alloc).map(_._1)
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("even?", 1, args.length))
}

object `max` extends StoreOperation("max", Some(2)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      {     val `a_pos` = args(0)._1
    val `a` = args(0)._2
    val `b_pos` = args(1)._1
    val `b` = args(1)._2
    `<`.call(fpos, (-1,-24), List((`a_pos`, `a`), (`b_pos`, `b`)), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        `b`
      }
      {
        `a`
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("max", 2, args.length))
}

object `min` extends StoreOperation("min", Some(2)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      {     val `a_pos` = args(0)._1
    val `a` = args(0)._2
    val `b_pos` = args(1)._1
    val `b` = args(1)._2
    `<`.call(fpos, (-1,-24), List((`a_pos`, `a`), (`b_pos`, `b`)), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        `a`
      }
      {
        `b`
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("min", 2, args.length))
}

object `abs` extends StoreOperation("abs", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `<`.call(fpos, (-1,-22), List((`x_pos`, `x`), ((0, 0), number(0))), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        `-`.call(fpos, (-1,-30), List(((0, 0), number(0)), (`x_pos`, `x`)), store, alloc).map(_._1)
      }
      {
        `x`
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("abs", 1, args.length))
}

object `gcd` extends SimpleFixpointPrimitiveUsingStore("gcd", Some(2)) {
  def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A,V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      {     val `a_pos` = args(0)._1
    val `a` = args(0)._2
    val `b_pos` = args(1)._1
    val `b` = args(1)._2
    `=`.call(fpos, (-1,-24), List((`b_pos`, `b`), ((0, 0), number(0))), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        `a`
      }
      {
        `modulo`.call(fpos, (-1,-41), List((`a_pos`, `a`), (`b_pos`, `b`)), store, alloc).map(_._1) >>= { `_1`  =>
          recursiveCall(List((`b_pos`, `b`), ((-1,-41), `_1`)))
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("gcd", 2, args.length))
}

object `lcm` extends StoreOperation("lcm", Some(2)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      {     val `m_pos` = args(0)._1
    val `m` = args(0)._2
    val `n_pos` = args(1)._1
    val `n` = args(1)._2
    `*`.call(fpos, (-1,-28), List((`m_pos`, `m`), (`n_pos`, `n`)), store, alloc).map(_._1) >>= { `_0`  =>
      `abs`.call(fpos, (-1,-23), List(((-1,-28), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `gcd`.call(fpos, (-1,-37), List((`m_pos`, `m`), (`n_pos`, `n`)), store, alloc).map(_._1) >>= { `_2`  =>
          `/`.call(fpos, (-1,-20), List(((-1,-23), `_1`), ((-1,-37), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("lcm", 2, args.length))
}

object `not` extends StoreOperation("not", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    ifThenElse(`x`)
    {
      bool(false)
    }
    {
      bool(true)
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("not", 1, args.length))
}

object `newline` extends StoreOperation("newline", Some(0)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 0) {
      {     bool(false) }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("newline", 0, args.length))
}

object `display` extends StoreOperation("display", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `x` }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("display", 1, args.length))
}

object `caar` extends StoreOperation("caar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `car`.call(fpos, (-1,-24), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-19), List(((-1,-24), `_0`)), store, alloc).map(_._1)
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("caar", 1, args.length))
}

object `cadr` extends StoreOperation("cadr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-24), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-19), List(((-1,-24), `_0`)), store, alloc).map(_._1)
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cadr", 1, args.length))
}

object `cddr` extends StoreOperation("cddr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-24), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-19), List(((-1,-24), `_0`)), store, alloc).map(_._1)
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cddr", 1, args.length))
}

object `cdar` extends StoreOperation("cdar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `car`.call(fpos, (-1,-24), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-19), List(((-1,-24), `_0`)), store, alloc).map(_._1)
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cdar", 1, args.length))
}

object `caaar` extends StoreOperation("caaar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `car`.call(fpos, (-1,-30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-25), List(((-1,-30), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-20), List(((-1,-25), `_1`)), store, alloc).map(_._1)
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("caaar", 1, args.length))
}

object `caadr` extends StoreOperation("caadr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-25), List(((-1,-30), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-20), List(((-1,-25), `_1`)), store, alloc).map(_._1)
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("caadr", 1, args.length))
}

object `cadar` extends StoreOperation("cadar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `car`.call(fpos, (-1,-30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-25), List(((-1,-30), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-20), List(((-1,-25), `_1`)), store, alloc).map(_._1)
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cadar", 1, args.length))
}

object `caddr` extends StoreOperation("caddr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-25), List(((-1,-30), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-20), List(((-1,-25), `_1`)), store, alloc).map(_._1)
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("caddr", 1, args.length))
}

object `cdaar` extends StoreOperation("cdaar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `car`.call(fpos, (-1,-30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-25), List(((-1,-30), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-20), List(((-1,-25), `_1`)), store, alloc).map(_._1)
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cdaar", 1, args.length))
}

object `cdadr` extends StoreOperation("cdadr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-25), List(((-1,-30), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-20), List(((-1,-25), `_1`)), store, alloc).map(_._1)
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cdadr", 1, args.length))
}

object `cddar` extends StoreOperation("cddar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `car`.call(fpos, (-1,-30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-25), List(((-1,-30), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-20), List(((-1,-25), `_1`)), store, alloc).map(_._1)
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cddar", 1, args.length))
}

object `cdddr` extends StoreOperation("cdddr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-30), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-25), List(((-1,-30), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-20), List(((-1,-25), `_1`)), store, alloc).map(_._1)
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cdddr", 1, args.length))
}

object `caaaar` extends StoreOperation("caaaar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `car`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `car`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("caaaar", 1, args.length))
}

object `caaadr` extends StoreOperation("caaadr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `car`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("caaadr", 1, args.length))
}

object `caadar` extends StoreOperation("caadar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `car`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `car`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("caadar", 1, args.length))
}

object `caaddr` extends StoreOperation("caaddr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `car`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("caaddr", 1, args.length))
}

object `cadaar` extends StoreOperation("cadaar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `car`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `car`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cadaar", 1, args.length))
}

object `cadadr` extends StoreOperation("cadadr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `car`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cadadr", 1, args.length))
}

object `caddar` extends StoreOperation("caddar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `car`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `car`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("caddar", 1, args.length))
}

object `cadddr` extends StoreOperation("cadddr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `car`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cadddr", 1, args.length))
}

object `cdaaar` extends StoreOperation("cdaaar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `car`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `cdr`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cdaaar", 1, args.length))
}

object `cdaadr` extends StoreOperation("cdaadr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `cdr`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cdaadr", 1, args.length))
}

object `cdadar` extends StoreOperation("cdadar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `car`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `cdr`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cdadar", 1, args.length))
}

object `cdaddr` extends StoreOperation("cdaddr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `car`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `cdr`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cdaddr", 1, args.length))
}

object `cddaar` extends StoreOperation("cddaar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `car`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `cdr`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cddaar", 1, args.length))
}

object `cddadr` extends StoreOperation("cddadr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `car`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `cdr`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cddadr", 1, args.length))
}

object `cdddar` extends StoreOperation("cdddar", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `car`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `cdr`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cdddar", 1, args.length))
}

object `cddddr` extends StoreOperation("cddddr", Some(1)) {
  override def call(fpos: Identity.Position, cpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 1) {
      {     val `x_pos` = args(0)._1
    val `x` = args(0)._2
    `cdr`.call(fpos, (-1,-36), List((`x_pos`, `x`)), store, alloc).map(_._1) >>= { `_0`  =>
      `cdr`.call(fpos, (-1,-31), List(((-1,-36), `_0`)), store, alloc).map(_._1) >>= { `_1`  =>
        `cdr`.call(fpos, (-1,-26), List(((-1,-31), `_1`)), store, alloc).map(_._1) >>= { `_2`  =>
          `cdr`.call(fpos, (-1,-21), List(((-1,-26), `_2`)), store, alloc).map(_._1)
        }
      }
    } }.map(x => (x, store))
    } else MayFail.failure(PrimitiveArityError("cddddr", 1, args.length))
}

object `equal?` extends SimpleFixpointPrimitiveUsingStore("equal?", Some(2)) {
  def callWithArgs(fpos: Identity.Position, cpos: Identity.Position, args: Args, store: Store[A,V], recursiveCall: Args => MayFail[V, Error], alloc: SchemeAllocator[A]): MayFail[(V, Store[A, V]), Error] =
    if (args.length == 2) {
      {     val `a_pos` = args(0)._1
    val `a` = args(0)._2
    val `b_pos` = args(1)._1
    val `b` = args(1)._2
    `eq?`.call(fpos, (-2,-16), List((`a_pos`, `a`), (`b_pos`, `b`)), store, alloc).map(_._1) >>= { `_0`  =>
      ifThenElse(`_0`)
      {
        bool(true)
      }
      {
        `null?`.call(fpos, (-3,-19), List((`a_pos`, `a`)), store, alloc).map(_._1) >>= { `_1`  =>
          ifThenElse(`_1`)
          {
            `null?`.call(fpos, (-3,-29), List((`b_pos`, `b`)), store, alloc).map(_._1)
          }