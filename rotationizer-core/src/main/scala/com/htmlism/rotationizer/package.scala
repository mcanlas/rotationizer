package com.htmlism

import cats.data._

package object rotationizer:
  extension [A](xs: NonEmptyList[A])
    def asList[B](f: List[A] => List[B]): NonEmptyList[B] =
      NonEmptyList.fromListUnsafe(f(xs.toList))
