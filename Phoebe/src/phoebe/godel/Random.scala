package phoebe.godel

import org.apache.commons.math.random._

object Random {
  implicit def enrichRandomGenerator(r: RandomGenerator) = new EnrichedRandomGenerator(r)

  implicit def enrichCorrelatedRandomVectorGenerator(r: CorrelatedRandomVectorGenerator) =
    new EnrichedCorrelatedRandomVectorGenerator(r)

  class EnrichedRandomGenerator private[Random](r: RandomGenerator) {
    def seed_=(seed: Int) {
      r.setSeed(seed)
    }

    def seed_=(seed: Int*) {
      r.setSeed(seed.toArray)
    }

    def seed_=(seed: Long) {
      r.setSeed(seed)
    }
  }


  class EnrichedCorrelatedRandomVectorGenerator private[Random](r: CorrelatedRandomVectorGenerator) {
    def generator = r.getGenerator
    def rootMatrix = r.getRootMatrix
    def rank = r.getRank
  }
}