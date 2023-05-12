package com.htmlism.rotationizer

import scala.util.Random
import scala.util.chaining._

import cats.Show
import cats.data.*
import cats.syntax.all.*

import com.htmlism.rotationizer.Rotation.Rotation7.OffCourtModifier.OutOnHitterSide
import com.htmlism.rotationizer.Rotation.Rotation7.OffCourtModifier.OutOnServerSide

sealed trait Rotation:
  def cycleRanks: NonEmptyVector[Int]

  def positionRanks: NonEmptyVector[Int]

object Rotation:
  def cycles[A](ranks: NonEmptyVector[Int], indices: NonEmptyList[A]): NonEmptyVector[NonEmptyVector[A]] =
    val xsSorted =
      indices
        .toNev
        .zipWith(ranks)(_ -> _)
        .sortBy(_._2)
        .map(_._1)

    xsSorted
      .zipWithIndex
      .map(_._2)
      .map(cycleFromOffset(xsSorted, _))

  def cycleFromOffset[A](xs: NonEmptyVector[A], offset: Int): NonEmptyVector[A] =
    val xsl =
      xs.toList

    val (left, right) =
      xsl.splitAt(offset)

    (left ::: right)
      .toVector
      .pipe(NonEmptyVector.fromVectorUnsafe)

  def courts(rotation: Rotation): NonEmptyVector[Court[CycleIndex]] =
    rotation match
      case Rotation6(xs, _) =>
        val cycleIndices =
          (0 until xs.length)
            .map(CycleIndex(_))
            .toList
            .pipe(NonEmptyList.fromListUnsafe)

        cycles(xs, cycleIndices)
          .map(cy => Court(cy, Nil, Nil))

      case Rotation7(xs, _, mod) =>
        val cycleIndices =
          (0 until xs.length)
            .map(CycleIndex(_))
            .toList
            .pipe(NonEmptyList.fromListUnsafe)

        cycles(xs, cycleIndices)
          .map { cy =>
            mod match
              case Rotation7.OffCourtModifier.OutOnServerSide =>
                val xs =
                  cy.toVector

                val onCourt =
                  (xs(0) +: xs.slice(2, xs.length))
                    .pipe(NonEmptyVector.fromVectorUnsafe)

                Court(onCourt, List(cy.toList(1)), Nil)

              case Rotation7.OffCourtModifier.OutOnHitterSide =>
                val xs =
                  cy.toVector

                val onCourt =
                  (xs.slice(0, 4) concat xs.slice(5, 7))
                    .pipe(NonEmptyVector.fromVectorUnsafe)

                Court(onCourt, Nil, List(xs(4)))
          }

      case Rotation10(xs, _) =>
        val cycleIndices =
          (0 until xs.length)
            .map(CycleIndex(_))
            .toList
            .pipe(NonEmptyList.fromListUnsafe)

        cycles(xs, cycleIndices)
          .map { cy =>
            val xs =
              cy.toVector

            val onCourt =
              ((xs(0) +: xs.slice(3, 6)) concat xs.slice(8, 10))
                .pipe(NonEmptyVector.fromVectorUnsafe)

            Court(onCourt, cy.toList.slice(1, 3), cy.toList.slice(6, 8))
          }

  case class Rotation6(
      cycleRanks: NonEmptyVector[Int],
      positionRanks: NonEmptyVector[Int]
  ) extends Rotation

  case class Rotation10(
      cycleRanks: NonEmptyVector[Int],
      positionRanks: NonEmptyVector[Int]
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

  object Rotation10:
    given RandomInstance[Rotation10] with
      def fromRng: Reader[Random, Rotation10] =
        (
          RandomInstance.ranks(10),
          RandomInstance.ranks(10)
        )
          .mapN(Rotation10.apply)

    given Show[Rotation10] =
      Show.fromToString

  case class Rotation7(
      cycleRanks: NonEmptyVector[Int],
      positionRanks: NonEmptyVector[Int],
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
