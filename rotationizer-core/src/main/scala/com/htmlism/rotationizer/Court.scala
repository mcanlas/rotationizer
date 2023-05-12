package com.htmlism.rotationizer

import cats.data.*

case class Court[A](xs: NonEmptyVector[A], serverSideOuts: List[A], hitterSideOuts: List[A]):
  assert(xs.length == 6, s"total number of players should be six, but got ${xs.length}")

  def position1: A =
    xs.getUnsafe(0)

  def position2: A =
    xs.getUnsafe(1)

  def position3: A =
    xs.getUnsafe(2)

  def position4: A =
    xs.getUnsafe(3)

  def position5: A =
    xs.getUnsafe(4)

  def position6: A =
    xs.getUnsafe(5)
