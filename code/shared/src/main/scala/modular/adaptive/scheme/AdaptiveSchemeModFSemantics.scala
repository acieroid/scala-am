package scalaam.modular.adaptive.scheme

import scalaam.modular.adaptive.{AdaptiveGlobalStore, AdaptiveModAnalysis, AdaptiveReturnValue}
import scalaam.language.scheme.SchemeExp
import scalaam.modular.scheme.StandardSchemeModFSemantics

/** Semantics for an adaptive Scheme MODF analysis. */
trait AdaptiveSchemeModFSemantics extends AdaptiveModAnalysis[SchemeExp]
                                     with AdaptiveGlobalStore[SchemeExp]
                                     with AdaptiveReturnValue[SchemeExp]
                                     with StandardSchemeModFSemantics
