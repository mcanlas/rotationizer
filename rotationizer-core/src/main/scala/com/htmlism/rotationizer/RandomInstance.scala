package com.htmlism.rotationizer

import scala.util.Random

import cats.data.*

trait RandomInstance[A]:
  def fromRng: Reader[Random, A]

object RandomInstance:
  def ranks(n: Int): Reader[Random, NonEmptyVector[Int]] =
    Reader { rng =>
      NonEmptyVector.fromVectorUnsafe(Vector.fill(n)(rng.nextInt))
    }

  def oneOf[A](xs: A*): Reader[Random, A] =
    Reader { rng =>
      xs(rng.nextInt(xs.length))
    }
