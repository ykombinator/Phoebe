package phoebe.godel

import phoebe.Prelude._
import phoebe.escher.Image
import marvin.image.MarvinImage

class Matrix[A] private[phoebe](val contents: Vector[Vector[A]])(implicit numericEv: Numeric[A]) extends ((Int, Int) =>? A) with Proxy {

  import numericEv._
  import phoebe.Prelude._

  // delegate `equals` and `hashCode` implementations to `contents`
  override def self = contents

  override def isDefinedAt(index: (Int, Int)): Boolean = {
    val (r, c) = index
    r >= 0 && r < nRows && c >= 0 && c < nColumns   
  }

  val nRows: Int = contents.length

  val nColumns: Int = contents(0).length.ensuring { len =>
    contents.forall(_.length == len)
  }

  def dimensions = (nRows, nColumns)

  def hasSameOrderAs[B : Numeric](that: Matrix[B]) = this.dimensions == that.dimensions

  def isComformableWith[B : Numeric](that: Matrix[B]) = this.nColumns == that.nRows

  def apply(index: (Int, Int)): A = contents(index._1)(index._2)

  def apply(r: Int, c: Int): A = contents(r)(c)

  def apply(r: Int): Vector[A] = contents(r)

  def updated[B >: A : Numeric](r: Int, c: Int, newValue: B): Matrix[B] = {
    val newContents = contents.updated(r, contents(r).updated(c, newValue))
    Matrix(newContents)
  }

  def replaceRow[B >: A : Numeric](r: Int, row: Vector[B]): Matrix[B] = {
    Matrix(contents.updated(r, row))
  }

  def replaceColumn[B >: A : Numeric](c: Int, column: Vector[B]): Matrix[B] = {
    val newContents = contents.zipWithIndex.map { case(row, r) =>
      row.updated(c, column(r))
    }
    Matrix(newContents)
  }

  def cofactor(rowNo: Int, columnNo: Int): Matrix[A] = {
    val cofactorContents = for {
      (row, r) <- contents.zipWithIndex
      if r != rowNo
    } yield for {
      (elem, c) <- row.zipWithIndex
      if c != columnNo
    } yield elem
    Matrix(cofactorContents)
  }

  def determinant: A = {
    assertIsSquare()
    nRows match {
      case 1 => this(0, 0)
      case 2 => this(0, 0) * this(1, 1) - this(1, 0) * this(0, 1)
      case n =>
        contents(0).zipWithIndex.map { case (elem, c) =>
           minor(0, c) * fromInt(math.pow(-1, c).toInt)
        }.sum
    }
  }

  def minor(rowNo: Int, columnNo: Int): A = {
    cofactor(rowNo, columnNo).determinant
  }

  def adjoint: Matrix[A] = {
    this.mapWithIndex { (_, r, c) =>
      minor(r, c) * fromInt(math.pow(-1, r + c).toInt)
    }.transpose
  }

  def isRowVector = nRows == 1

  def isColumnsVector = nColumns == 1

  def isSquare = nRows == nColumns

  def isSymmetric: Boolean = {
    assertIsSquare()
    val contentsIndexed = for {
      (row, r) <- contents.zipWithIndex
      (elem, c) <- row.zipWithIndex
    } yield (elem, r, c)

    contentsIndexed.groupBy { case (_, r, c) =>
      (r + c, (r - c).abs)
    } map { case (_, vec) =>
      vec.map(_._1)
    } forall(_.distinct.size == 1)
  }

  private def assertIsSquare() {
    assert(this.isSquare, "Not a square matrix.")
  }

  private def assertSameOrder[B : Numeric](that: Matrix[B]) {
    assert(this.hasSameOrderAs(that), "Matrices differ in dimensions.")
  }

  def diagonal: Vector[A] = {
    assertIsSquare()
    for {
      (row, r) <- contents.zipWithIndex
      (elem, c) <- row.zipWithIndex
      if r == c
    } yield elem
  }

  def trace: A = diagonal.sum

  def forall(f: A => Boolean): Boolean = {
    contents.forall { row =>
      row.forall(f)
    }
  }

  def exists(f: A => Boolean): Boolean = {
    contents.exists { row =>
      row.exists(f)
    }
  }

  def forallWithIndex(f: (A, Int, Int) => Boolean): Boolean = {
    contents.zipWithIndex.forall { case (row, r) =>
      row.zipWithIndex.forall { case (elem, c) =>
        f(elem, r, c)
      }
    }
  }

  def existsWithIndex(f: (A, Int, Int) => Boolean): Boolean = {
    contents.zipWithIndex.exists { case (row, r) =>
      row.zipWithIndex.exists { case (elem, c) =>
        f(elem, r, c)
      }
    }
  }

  def isDiagonal: Boolean = this.isSquare && this.forallWithIndex { (elem, r, c) =>
    r == c || elem == 0
  }

  def isScalar: Boolean = this.isDiagonal && diagonal.hasAllElementsEqual

  //
  // I sense something fishy in here. I better check out Sobral's Matrix implementation before rolling out the final stuff.
  //

  def isZero = this.forall(_ == 0)

  def isIdentity = this.isSquare && this.forallWithIndex { (elem, r, c) =>
    (r == c && elem == 1) || (r != c && elem == 0)
  }

  def zipWith[B : Numeric, C : Numeric](that: Matrix[B])(f: (A, B) => C): Matrix[C] = {
    assertSameOrder(that)
    val zippedContents = (contents, that.contents).zipped.map((v1, v2) => (v1, v2).zipped.map(f))
    Matrix(zippedContents)
  }

  def map[B : Numeric](f: A => B): Matrix[B] = {
    Matrix(contents.map(_.map(f)))
  }

  def mapWithIndex[B : Numeric](f: (A, Int, Int) => B): Matrix[B] = {
    val newContents = for {
      (row, r) <- contents.zipWithIndex
    } yield for {
      (elem, c) <- row.zipWithIndex
    } yield f(elem, r, c)
    Matrix(newContents)
  }

  // Ask at StackOverflow why covariance didn't work here.

  def +(that: Matrix[A]): Matrix[A] = this.zipWith(that)(_ + _)

  def -(that: Matrix[A]): Matrix[A] = this.zipWith(that)(_ - _)

  def *(scalar: A): Matrix[A] = this.map(_ * scalar)

  def *(that: Matrix[A]): Matrix[A] = {
    assert(this.isComformableWith(that))
    Matrix.tabulate(this.nRows, that.nColumns) { (r, c) =>
      (this(r), that.transpose(c)).zipped.map(_ * _).sum
    }
  }

  def transpose: Matrix[A] = Matrix(contents.transpose)

  def negative: Matrix[A] = this.map(_ * fromInt(-1))

  def rows = contents.iterator

  def columns = contents.transpose.iterator

  def foreach(f: A => Unit): Unit = {
    contents.flatten.foreach(f)
  }

  override def toString = contents.map(_.mkString("[", " ", "]")).mkString("\n")
}

object Matrix {
  def apply[A : Numeric](rows: Vector[A]*): Matrix[A] = Matrix(Vector(rows: _*))

  def apply[A : Numeric](contents: Vector[Vector[A]]): Matrix[A] = new Matrix(contents)

  def fill[A : Numeric](nRows: Int, nColumns: Int)(value: A): Matrix[A] = {
    val contents = Vector.fill(nRows)(Vector.fill(nColumns)(value))
    Matrix(contents)
  }

  def tabulate[A : Numeric](nRows: Int, nColumns: Int)(f: (Int, Int) => A): Matrix[A] = {
    Matrix(Vector.tabulate(nRows, nColumns)(f))
  }

  def zero[A : Numeric](nRows: Int, nColumns: Int): Matrix[A] = Matrix.fill(nRows, nColumns)(implicitly[Numeric[A]].zero)

  def identity[A : Numeric](size: Int): Matrix[A] = Matrix.tabulate(size, size) { (r, c) =>
    val numericEv = implicitly[Numeric[A]]
    if(r == c)
      numericEv.one
    else
      numericEv.zero
  }
}