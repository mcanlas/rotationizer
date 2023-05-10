package com.htmlism.rotationizer

import cats.data.*

case class Court[A](xs: NonEmptyList[A], serverSideOuts: List[A], hitterSideOuts: List[A]):
  assert(xs.length == 6)

  private lazy val xsl =
    xs.toList

  def position1: A =
    xsl(0)

  def position2: A =
    xsl(1)

  def position3: A =
    xsl(2)

  def position4: A =
    xsl(3)

  def position5: A =
    xsl(4)

  def position6: A =
    xsl(5)
