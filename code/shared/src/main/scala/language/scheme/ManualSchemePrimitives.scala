class ManualSchemePrimitives[V, A <: Address](implicit val schemeLattice: SchemeLattice[V, A, SchemePrimitive[V,A], _]) {

  /** Bundles all the primitives together, annotated with R5RS support (v: supported, vv: supported and tested in PrimitiveTests, vx: not fully supported, x: not supported), and section in Guile manual */
  def allPrimitives: List[SchemePrimitive[V,A]] = {
    import PrimitiveDefs._
    List(
      Abs, /* [vv] abs: Arithmetic */
      Append,
      Assoc, /* [vv] assoc: Retrieving Alist Entries */
      Assq, /* [vv] assq: Retrieving Alist Entries */
      Equal, /* [vx] equal?: Equality */
      Evenp, /* [vv] even?: Integer Operations */
      Gcd, /* [vx] gcd: Integer Operations */
      Length, /* [vv] length: List Selection */
      Listp,
      Max, /* [vv] max: Arithmetic */
      Member, /* [vv] member: List Searching */
      Memq, /* [v]  memq: List Searching */
      Min, /* [vv] min: Arithmetic */
      Negativep, /* [vv] negative?: Comparison */
      Newline, /* [v]  newline: Writing */
      Not, /* [vv] not: Booleans */
      NumberToString, /* [vx] number->string: Conversion: does not support two arguments */
      Oddp, /* [vv] odd?: Integer Operations */
      Positivep, /* [vv] positive?: Comparison */
      Zerop, /* [vv] zero?: Comparison */
      LessOrEqual, /* [vv]  <= */
      GreaterOrEqual, /* [vv]  >= */
      Caar,
      Cadr, /* [v]  caar etc. */
      Cdar,
      Cddr,
      Caaar,
      Caadr,
      Cadar,
      Caddr,
      Cdaar,
      Cdadr,
      Cddar,
      Cdddr,
      Caaaar,
      Caaadr,
      Caadar,
      Caaddr,
      Cadaar,
      Cadadr,
      Caddar,
      Cadddr,
      Cdaaar,
      Cdaadr,
      Cdadar,
      Cdaddr,
      Cddaar,
      Cddadr,
      Cdddar,
      Cddddr,
    ) ++ SchemeLatticePrimitives.allPrimitives
  }

  object PrimitiveDefs extends PrimitiveBase {

    import SchemeLattice._

    type V = V
    type A = A

    private trait Clause {
      def otherwise(act: => MayFail[V,Error]): MayFail[V,Error]
      def otherwise(cls: Clause): Clause = otherwise_(cls, this)
      def otherwise_(cls: Clause, parent: Clause) = new Clause {
        def otherwise(alt: => MayFail[V,Error]) = parent.otherwise(cls.otherwise(alt))
      }
    }

    private def ifV(prd: => MayFail[V,Error])(csq: => MayFail[V,Error]) = new Clause {
      def otherwise(alt: => MayFail[V,Error]) = ifThenElse(prd) { csq } { alt }
    }

    object LessOrEqual extends NoStoreOperation("<=", Some(2)) {
      override def call2(x: V, y: V) = (or _)(lt(x, y), numEq(x, y)) /*for {
        ltres <- lt(x, y)
        eqres <- numEq(x, y)
      } yield or(ltres, eqres) */
    }
    object GreaterOrEqual extends NoStoreOperation(">=", Some(2)) {
      override def call2(x: V, y: V) = not(LessThan.call2(x, y))
    }
    
    /** (define (zero? x) (= x 0)) */
    object Zerop extends NoStoreOperation("zero?", Some(1)) {
      override def call(x: V) = numEq(number(0), x)
    }

    /** (define (positive? x) (< x 0)) */
    object Positivep extends NoStoreOperation("positive?", Some(1)) {
      override def call(x: V) = lt(number(0), x)
    }

    /** (define (positive? x) (< 0 x)) */
    object Negativep extends NoStoreOperation("negative?", Some(1)) {
      override def call(x: V) = lt(x, number(0))
    }

    /** (define (odd? x) (= 1 (modulo x 2))) */
    object Oddp extends NoStoreOperation("odd?", Some(1)) {
      override def call(x: V) = modulo(x, number(2)) >>= (numEq(number(1), _))
    }

    /** (define (even? x) (= 0 (modulo x 2))) */
    object Evenp extends NoStoreOperation("even?", Some(1)) {
      override def call(x: V) = modulo(x, number(2)) >>= (numEq(number(0), _))
    }
    object Max extends NoStoreOperation("max") {
      /* TODO: In Scheme, max casts numbers to inexact as soon as one of them is inexact, but we don't support that */
      private def call(args: List[V], max: V): MayFail[V, Error] = args match {
        case Nil => max
        case x :: rest =>
          ifThenElse(lt(max, x)) {
            call(rest, x)
          } {
            call(rest, max)
          }
      }
      override def call(args: List[V]) = args match {
        case Nil       => MayFail.failure(PrimitiveVariadicArityError(name, 1, 0))
        case x :: rest => call(rest, x)
      }
    }
    object Min extends NoStoreOperation("min") {
      /* TODO: same remark as max */
      private def call(args: List[V], min: V): MayFail[V, Error] = args match {
        case Nil => min
        case x :: rest =>
          ifThenElse(lt(x, min)) {
            call(rest, x)
          } {
            call(rest, min)
          }
      }
      override def call(args: List[V]) = args match {
        case Nil       => MayFail.failure(PrimitiveVariadicArityError(name, 1, 0))
        case x :: rest => call(rest, x)
      }
    }

    /** (define (abs x) (if (< x 0) (- 0 x) x)) */
    object Abs extends NoStoreOperation("abs", Some(1)) {
      override def call(x: V) =
        ifThenElse(lt(x, number(0))) {
          minus(number(0), x)
        } {
          x
        }
    }

    object Gcd extends SimpleFixpointPrimitive("gcd", Some(2)) {
      def callWithArgs(args: Args, gcd: Args => MayFail[V, Error]): MayFail[V, Error] = {
        val a :: b :: Nil = args
        ifThenElse(numEq(b, number(0))) { a } {
          modulo(a, b) >>= (amodb => gcd(List(b, amodb)))
        }
      }
    }

    object Not extends NoStoreOperation("not", Some(1)) {
      override def call(x: V) = not(x)
    }


    class CarCdrOperation(override val name: String) extends StoreOperation(name, Some(1)) {
      trait Spec
      case object Car extends Spec
      case object Cdr extends Spec
      val spec: List[Spec] = name
        .drop(1)
        .take(name.length - 2)
        .toList
        .reverseIterator
        .map(
          c =>
            if (c == 'a') {
              Car
            } else if (c == 'd') {
              Cdr
            } else {
              throw new Exception(s"Incorrect car/cdr operation: $name")
            }
        ).toList
      override def call(v: V, store: Store[A, V]) =
        for {
          v <- spec.foldLeft(MayFail.success[V, Error](v))(
            (acc, op) =>
              for {
                v <- acc
                res <- dereferencePointer(v, store) { consv =>
                  op match {
                    case Car => car(consv)
                    case Cdr => cdr(consv)
                  }
                }
              } yield res
          )
        } yield (v, store)
    }

    object Car    extends CarCdrOperation("car")
    object Cdr    extends CarCdrOperation("cdr")
    object Caar   extends CarCdrOperation("caar")
    object Cadr   extends CarCdrOperation("cadr")
    object Cdar   extends CarCdrOperation("cdar")
    object Cddr   extends CarCdrOperation("cddr")
    object Caaar  extends CarCdrOperation("caaar")
    object Caadr  extends CarCdrOperation("caadr")
    object Cadar  extends CarCdrOperation("cadar")
    object Caddr  extends CarCdrOperation("caddr")
    object Cdaar  extends CarCdrOperation("cdaar")
    object Cdadr  extends CarCdrOperation("cdadr")
    object Cddar  extends CarCdrOperation("cddar")
    object Cdddr  extends CarCdrOperation("cdddr")
    object Caaaar extends CarCdrOperation("caaaar")
    object Caaadr extends CarCdrOperation("caaadr")
    object Caadar extends CarCdrOperation("caadar")
    object Caaddr extends CarCdrOperation("caaddr")
    object Cadaar extends CarCdrOperation("cadaar")
    object Cadadr extends CarCdrOperation("cadadr")
    object Caddar extends CarCdrOperation("caddar")
    object Cadddr extends CarCdrOperation("cadddr")
    object Cdaaar extends CarCdrOperation("cdaaar")
    object Cdaadr extends CarCdrOperation("cdaadr")
    object Cdadar extends CarCdrOperation("cdadar")
    object Cdaddr extends CarCdrOperation("cdaddr")
    object Cddaar extends CarCdrOperation("cddaar")
    object Cddadr extends CarCdrOperation("cddadr")
    object Cdddar extends CarCdrOperation("cdddar")
    object Cddddr extends CarCdrOperation("cddddr")

    object SetCar extends StoreOperation("set-car!", Some(2)) {
      override def call(cell: V, value: V, store: Store[A, V]) =
        getPointerAddresses(cell)
          .foldLeft(MayFail.success[Store[A, V], Error](store))(
            (acc, a) =>
              for {
                consv <- store.lookupMF(a) /* look up in old store */
                st    <- acc /* updated store */
                v1 = value /* update car */
                v2 <- cdr(consv) /* preserves cdr */
              } yield st.update(a, cons(v1, v2))
          )
          .map(store => (bool(false) /* undefined */, store))
    }
    object SetCdr extends StoreOperation("set-cdr!", Some(2)) {
      override def call(cell: V, value: V, store: Store[A, V]) =
        getPointerAddresses(cell)
          .foldLeft(MayFail.success[Store[A, V], Error](store))(
            (acc, a) =>
              for {
                consv <- store.lookupMF(a) /* look up in old store */
                st    <- acc /* updated store */
                v1    <- car(consv) /* preserves car */
                v2 = value /* update cdr */
              } yield st.update(a, cons(v1, v2))
          )
          .map(store => (bool(false) /* undefined */, store))
    }

    /** (define (equal? a b)
          (or (eq? a b)
            (and (null? a) (null? b))
            (and (pair? a) (pair? b) (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
            (and (vector? a) (vector? b)
              (let ((n (vector-length a)))
                (and (= (vector-length b) n)
                  (letrec ((loop (lambda (i)
                                   (or (= i n)
                                     (and (equal? (vector-ref a i) (vector-ref b i))
                                       (loop (+ i 1)))))))
                    (loop 0)))))))
      */
    // TODO: this is without vectors
    object Equal extends StoreOperation("equal?", Some(2)) {
      override def call(a: V, b: V, store: Store[A, V]) = {
        def equalp(a: V, b: V, visited: Set[(V, V)]): TailRec[MayFail[V, Error]] = {
          if (visited.contains((a, b)) || a == bottom || b == bottom) {
            done(bottom)
          } else {
            val visited2 = visited + ((a, b))
            ifThenElseTR(eqq(a, b)) {
              /* If a and b are eq?, then they are equal? */
              done(bool(true))
            } {
              ifThenElseTR((and _)(isNull(a), isNull(b))) {
                /* If both a and b are null, then they are equal? */
                done(bool(true))
              } {
                ifThenElseTR((and _)(isCons(a), isCons(b))) {
                  /* If both cons, check car and cdr */
                  liftTailRec(
                    car(a).flatMap(
                      cara =>
                        car(b).flatMap(
                          carb =>
                            cdr(a).flatMap(
                              cdra =>
                                cdr(b).flatMap(
                                  cdrb =>
                                    tailcall(equalp(cara, carb, visited2)).flatMap(
                                      eqcar =>
                                        tailcall(equalp(cdra, cdrb, visited2)).map(
                                          eqcdr =>
                                            for {
                                              x <- eqcar
                                              y <- eqcdr
                                            } yield and(x, y)
                                        )
                                    )
                                )
                            )
                        )
                    )
                  )
                } {
                  ifThenElseTR((and _)(isPointer(a), isPointer(b))) {
                    /* both pointers, then look up their values */
                    dereferencePointerTR(a, store) { consa =>
                      dereferencePointerTR(b, store) { consb =>
                        tailcall(equalp(consa, consb, visited2))
                      }
                    }
                  } {
                    /* otherwise, there is no equality possible */
                    done(bool(false))
                  }
                }
              }
            }
          }
        }
        equalp(a, b, Set()).result.map(v => (v, store))
      }
    }

    /** (define (length l)
          (if (null? l)
              0
              (+ 1 (length (cdr l)))))
    */
    object Length extends FixpointPrimitiveUsingStore("length",Some(1)) {
      /** the argument to length is just the current pair */
      type Args = V
      def initialArgs(fexp: Identity.Position, args: List[(Identity.Position, V)]) = args match {
        case (_, arg) :: Nil  => Some(arg)
        case _                => None
      }
      /** calls are just that argument */
      type Call = Args
      def callFor(args: V) = args
      /** since Args == Calls, the argument remains the same */
      def updateArgs(oldArgs: V, newArgs: V) = { assert(newArgs == oldArgs) ; oldArgs }
      /** The actual implementation of the primitive */
      override def callWithArgs(l: V)(alloc: SchemeAllocator[A], store: Store[A,V], length: V => MayFail[V,Error]): MayFail[V,Error] =
        ifV(isNull(l)) {
          // if we have l = '(), length(l) = 0
          number(0)
        } otherwise ifV(isPointer(l)) {
          // if we have l = cons(a,d), length(l) = length(d) + 1
          dereferencePointer(l, store) { consv =>
            for {
              next      <- cdr(consv)
              len_next  <- length(next)
              result    <- plus(len_next, number(1))
            } yield result
          }
        } otherwise {
          // if we have have something else (i.e., not a list), length throws a type error!
          MayFail.failure(PrimitiveNotApplicable("length", List(l)))
        }
    }
    /** (define (append l1 l2)
          (if (null? l1)
              l2
              (cons (car l1)
                    (append (cdr l1) l2))))
    */
    object Append extends FixpointPrimitiveUsingStore("append", Some(2)) {
      /** the arguments to append are the two given input arguments + the 'append expression' and 'the current index' into the list (used for allocation) */
      type Args = (V,V,Identity.Position,Int)
      def initialArgs(fexp: Identity.Position, args: List[(Identity.Position, V)]) = args match {
        case (_, l1) :: (_, l2) :: Nil  => Some((l1, l2, fexp, 0))
        case _                          => None
      }
      /** calls only take into account the current pair (= first argument of append) */
      type Call = V
      def callFor(args: Args) = args._1
      /** arguments: ignore changes to the index to ensure termination (and other args are guaranteed to be the same anyway)*/
      def updateArgs(oldArgs: Args, newArgs: Args) = oldArgs
      /** The actual implementation of append */
      override def callWithArgs(args: Args)(alloc: SchemeAllocator[A], store: Store[A,V], append: Args => MayFail[V,Error]): MayFail[V,Error] = args match {
        case (l1, l2, fexp, idx) =>
          ifV(isNull(l1)) {
            // if we have l1 = '(), append(l1,l2) = l2
            l2
          } otherwise ifV(isPointer(l1)) {
            // if we have l1 = cons(a,d), append(l1,l2) = cons(a,append(d,l2))
            val addr = alloc.pointer((fexp, fexp), idx)
            dereferencePointer(l1, store) { consv =>
              for {
                carv      <- car(consv)
                cdrv      <- cdr(consv)
                app_next  <- append((cdrv, l2, fexp, idx + 1))
                result    <- cons(carv, app_next)
              } yield {
                store.extend(addr, result)
                pointer(addr)
              }
            }
          } otherwise {
            // if we have have something else (i.e., not a list), append throws a type error!
            MayFail.failure(PrimitiveNotApplicable("length", List(l1)))
          }
      }
    }

    /** (define list (lambda args
          (if (null? args)
            '()
            (if (pair? args)
              (cons (car args) (apply list (cdr args)))
              args))))
      */
    object ListPrim extends StoreOperation("list", None) {
      override def call(fpos: Identity.Position, args: List[(Identity.Position, V)], store: Store[A, V], alloc: SchemeAllocator[A]) =
        args match {
          case Nil => (nil, store)
          case (exp, v) :: rest =>
            for {
              (restv, store2) <- call(fpos, rest, store, alloc)
              consv  = cons(v, restv)
              consa  = alloc.pointer((exp, fpos))
              store3 = store2.extend(consa, consv)
            } yield (pointer(consa), store3)
        }
    }

    /** (define (list? l) (or (and (pair? l) (list? (cdr l))) (null? l))) */
    object Listp extends StoreOperation("list?", Some(1)) {
      override def call(l: V, store: Store[A, V]) = {
        def listp(l: V, visited: Set[V]): TailRec[MayFail[V, Error]] = {
          if (visited.contains(l) || l == bottom) {
            /* R5RS: "all lists have finite length", and the cases where this is reached
             * include circular lists. If an abstract list reaches this point, it
             * may be also finite but will reach a true branch somewhere else, and
             * both booleans will get joined */
            done(bool(false))
          } else {
            ifThenElseTR(isNull(l)) {
              done(bool(true))
            } {
              ifThenElseTR(isCons(l)) {
                /* This is a cons, check that the cdr itself is a list */
                liftTailRec(cdr(l) >>= (cdrl => tailcall(listp(cdrl, visited + l))))
              } {
                ifThenElseTR(isPointer(l)) {
                  /* This is a pointer, dereference it and check if it is itself a list */
                  dereferencePointerTR(l, store) { consv =>
                    tailcall(listp(consv, visited + l))
                  }
                } {
                  /* Otherwise, not a list */
                  done(bool(false))
                }
              }
            }
          }
        }
        listp(l, Set()).result.map(v => (v, store))
      }
    }

    /** (define (list-ref l index)
          (if (pair? l)
            (if (= index 0)
              (car l)
              (list-ref (cdr l) (- index 1)))
            (error "list-ref applied to a non-list"))) */
    object ListRef extends StoreOperation("list-ref", Some(2)) {
      override def call(l: V, index: V, store: Store[A, V]) = {
        def listRef(l: V, index: V, visited: Set[(V, V)]): TailRec[MayFail[V, Error]] = {
          if (visited.contains((l, index)) || l == bottom || index == bottom) {
            done(bottom)
          } else {
            ifThenElseTR(isPointer(l)) {
              // dereferences the pointer and list-ref that
              dereferencePointerTR(l, store) { consv =>
                tailcall(listRef(consv, index, visited + ((l, index))))
              }
            } {
              ifThenElseTR(isCons(l)) {
                ifThenElseTR(numEq(index, number(0))) {
                  // index is 0, return car
                  done(car(l))
                } {
                  // index is >0, decrease it and continue looking into the cdr
                  liftTailRec(
                    cdr(l) >>= (
                        cdrl =>
                          minus(index, number(1)) >>= (
                              index2 => tailcall(listRef(cdrl, index2, visited + ((l, index))))
                          )
                      )
                  )
                }
              } {
                // not a list
                done(MayFail.failure(PrimitiveNotApplicable("list-ref", List(l, index))))
              }
            }
          }
        }
        listRef(l, index, Set.empty).result.map(v => (v, store))
      }
    }

    /** (define (member e l) ; member, memq and memv are similar, the difference lies in the comparison function used
          (if (null? l)
            #f
            (if (equal? (car l) e)
              l
              (member e (cdr l))))) */
    abstract class MemberLike(
        override val name: String,
        eqFn: (V, V, Store[A, V]) => MayFail[V, Error]
    ) extends StoreOperation(name, Some(2)) {
      override def call(e: V, l: V, store: Store[A, V]) = {
        def mem(e: V, l: V, visited: Set[V]): TailRec[MayFail[V, Error]] = {
          if (visited.contains(l) || e == bottom || l == bottom) {
            done(bottom)
          } else {
            ifThenElseTR(isNull(l)) {
              /* list is empty, return false */
              done(bool(false))
            } {
              ifThenElseTR(isPointer(l)) {
                dereferencePointerTR(l, store) { lv =>
                  liftTailRec(
                    car(lv) >>= (
                        carl =>
                          ifThenElseTR(eqFn(e, carl, store)) {
                            /* (car l) and e are equal, return l */
                            done(l)
                          } {
                            liftTailRec(cdr(lv) >>= (cdrl => tailcall(mem(e, cdrl, visited + l))))
                          }
                      )
                  )
                }
              } {
                /* not a list. Note: it may be a cons, but cons shouldn't come from the outside
                 * as they are wrapped in pointers, so it shouldn't happen that
                 * l is a cons at this point */
                done(MayFail.failure(PrimitiveNotApplicable(name, List(e, l))))
              }
            }
          }
        }
        mem(e, l, Set.empty).result.map(v => (v, store))
      }
    }

    object Member
        extends MemberLike(
          "member",
          (x: V, y: V, store: Store[A, V]) => Equal.call(x, y, store).map(_._1)
        )
    object Memq extends MemberLike("memq", (x: V, y: V, store: Store[A, V]) => Eq.call2(x, y))

    abstract class AssocLike(
        override val name: String,
        eqFn: (V, V, Store[A, V]) => MayFail[V, Error]
    ) extends StoreOperation(name, Some(2)) {
      override def call(e: V, l: V, store: Store[A, V]) = {
        def assoc(e: V, l: V, visited: Set[V]): TailRec[MayFail[V, Error]] = {
          if (visited.contains(l) || e == bottom || l == bottom) {
            done(bottom)
          } else {
            ifThenElseTR(isNull(l)) {
              done(bool(false))
            } {
              ifThenElseTR(isPointer(l)) {
                dereferencePointerTR(l, store) { lv =>
                  liftTailRec(
                    car(lv) >>= (
                        carl =>
                          ifThenElseTR(isPointer(carl)) {
                            dereferencePointerTR(carl, store) { carlv =>
                              liftTailRec(
                                car(carlv) >>= (
                                    caarl =>
                                      ifThenElseTR(eqFn(e, caarl, store)) {
                                        done(carl)
                                      } {
                                        liftTailRec(
                                          cdr(lv) >>= (
                                              cdrl => tailcall(assoc(e, cdrl, visited + l))
                                          )
                                        )
                                      }
                                  )
                              )
                            }
                          } {
                            done(MayFail.failure(PrimitiveNotApplicable(name, List(e, l))))
                          }
                      )
                  )
                }
              } {
                done(MayFail.failure(PrimitiveNotApplicable(name, List(e, l))))
              }
            }
          }
        }
        assoc(e, l, Set.empty).result.map(v => (v, store))
      }
    }
    object Assoc
        extends AssocLike(
          "assoc",
          (x: V, y: V, store: Store[A, V]) => Equal.call(x, y, store).map(_._1)
        )
    object Assq extends AssocLike("assq", (x: V, y: V, store: Store[A, V]) => Eq.call2(x, y))

  }
}