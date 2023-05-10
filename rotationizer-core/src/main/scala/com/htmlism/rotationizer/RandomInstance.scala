package com.htmlism.rotationizer

import scala.util.Random

import cats.data.*

trait RandomInstance[A]:
  def fromRng: Reader[Random, A]

object RandomInstance:
  def ranks(n: Int): Reader[Random, NonEmptyList[Int]] =
    Reader { rng =>
      NonEmptyList.fromListUnsafe(List.fill(n)(rng.nextInt))
    }

  def oneOf[A](xs: A*): Reader[Random, A] =
    Reader { rng =>
      xs(rng.nextInt(xs.length))
    }
