package com.htmlism.rotationizer

import scala.util.Random
import scala.util.chaining._

import cats.Show
import cats.data.*
import cats.syntax.all.*

import com.htmlism.rotationizer.Rotation.Rotation7.OffCourtModifier.OutOnHitterSide
import com.htmlism.rotationizer.Rotation.Rotation7.OffCourtModifier.OutOnServerSide

sealed trait Rotation:
  def cycleRanks: NonEmptyList[Int]

  def positionRanks: NonEmptyList[Int]

object Rotation:
  def cycles[A](ranks: NonEmptyList[Int], indices: NonEmptyList[A]): NonEmptyList[NonEmptyList[A]] =
    val xsSorted =
      indices
        .zip(ranks)
        .sortBy(_._2)
        .map(_._1)

    xsSorted
      .toList
      .indices
      .map { i =>
        cycleFromOffset(xsSorted, i)
      }
      .toList
      .pipe(NonEmptyList.fromListUnsafe)

  def cycleFromOffset[A](xs: NonEmptyList[A], offset: Int): NonEmptyList[A] =
    val ys =
      xs.toList

    val (left, right) =
      ys.splitAt(offset)

    NonEmptyList.fromListUnsafe(left ::: right)

  def courts(rotation: Rotation): NonEmptyList[Court[Int]] =
    rotation match
      case Rotation6(xs, _) =>
        cycles(xs, (0 until xs.length).toList.pipe(NonEmptyList.fromListUnsafe))
          .map(cy => Court(cy, Nil, Nil))

      case Rotation7(xs, _, _) =>
        cycles(xs, (0 until xs.length).toList.pipe(NonEmptyList.fromListUnsafe))
          .map(cy => Court(cy, Nil, Nil))

  case class Rotation6(
      cycleRanks: NonEmptyList[Int],
      positionRanks: NonEmptyList[Int]
  ) extends Rotation

  object Rotation6:
    given RandomInstance[Rotation6] with
      def fromRng: Reader[Random, Rotation6] =
        (
          RandomInstance.ranks(6),
          RandomInstance.ranks(6)
        )
          .mapN(Rotation6.apply)

    given Show[Rotation6] =
      Show.fromToString

  case class Rotation7(
      cycleRanks: NonEmptyList[Int],
      positionRanks: NonEmptyList[Int],
      offCourtModifier: Rotation7.OffCourtModifier
  ) extends Rotation

  object Rotation7:
    enum OffCourtModifier:
      case OutOnServerSide
      case OutOnHitterSide

    given RandomInstance[Rotation7] with
      def fromRng: Reader[Random, Rotation7] =
        (
          RandomInstance.ranks(7),
          RandomInstance.ranks(7),
          RandomInstance.oneOf(OutOnServerSide, OutOnHitterSide)
        )
          .mapN(Rotation7.apply)

    given Show[Rotation7] =
      Show.fromToString
