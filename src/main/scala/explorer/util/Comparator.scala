package explorer.util

import block.PowBlock

object Comparator {
  def blockEquals(idHash: String): PowBlock => Boolean = {
    powBlock: PowBlock => (powBlock.idString equals idHash)
  }

  def blockEquals(number: Long): PowBlock => Boolean = {
    powBlock: PowBlock => (powBlock.number equals number)
  }
}
