package phoebe.godel

import phoebe.Prelude._

case class Vector3D(x: Double = 0.0, y: Double = 0.0, z: Double = 0.0) {
  override def toString = "%.2fi %+.2fj %+.2fk" format (x, y, z)

  def isNaN = List(x, y, z).exists(_.isNaN)

  def isInfinite = !isNaN && List(x, y, x).exists(_.isInfinite)

  def norm1 = List(x, y, z).map(_.abs).sum

  def normal = math.sqrt(normSq)

  def normSq = x ~^ 2 + y ~^ 2 + z ~^ 2

  def normInf = List(x, y, x).map(_.abs).max

  def alpha = math.atan2(y, x)

  def delta = math.asin(z / normal)

  def +(that: Vector3D) = Vector3D(x + that.x, y + that.y, z + that.z)

  def -(that: Vector3D) = Vector3D(x - that.x, y - that.y, z - that.z)

  def o(that: Vector3D) = x * that.x + y * that.y + z * that.z

  def *(that: Vector3D) = Vector3D(
    this.y * that.z - this.z * that.y,
    this.z * that.x - this.x * that.z,
    this.x * that.y - this.y * that.x
  )

  def ~(that: Vector3D) = math.sqrt((that.x - this.x) ~^ 2 + (that.y - this.y) ~^ 2 + (that.z - this.z) ~^ 2)
}

object Vector3D {
  val Zero = Vector3D(0.0, 0.0, 0.0)
  val I = Vector3D(1.0, 0.0, 0.0)
  val MinusI = Vector3D(-1.0, 0.0, 0.0)
  val J = Vector3D(0.0, 1.0, 0.0)
  val MinusJ = Vector3D(0.0, -1.0, 0.0)
  val K = Vector3D(0.0, 0.0, 1.0)
  val MinusK = Vector3D(0.0, 0.0, -1.0)
  val NaN = Vector3D(Double.NaN, Double.NaN, Double.NaN)
  val PositiveInfinity = Vector3D(Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity)
  val NegativeInfinity = Vector3D(Double.NegativeInfinity, Double.NegativeInfinity, Double.NegativeInfinity)
}