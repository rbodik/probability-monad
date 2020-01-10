package probability_monad

import Distribution._

object MyExamples extends App {
  case class Trial(haveFairCoin: Boolean, flips: List[Coin])

  def bayesianCoin(nflips: Int): Distribution[Trial] = {
    for {
      haveFairCoin <- tf()
      c = if (haveFairCoin) coin else biasedCoin(0.6)
      flips <- c.repeat(nflips)
    } yield Trial(haveFairCoin, flips)
  }

  print(bayesianCoin(5).given(_.flips.forall(_ == H)).pr(_.haveFairCoin))
}
