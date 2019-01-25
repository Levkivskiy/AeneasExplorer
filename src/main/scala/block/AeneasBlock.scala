package block

import commons.SimpleBoxTransaction
import scorex.core.block.Block
import scorex.core.mainviews.PersistentNodeViewModifier
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

/**
  * @author is Alex Syrotenko (@flystyle)
  *         Created on 19.01.18.
  */
trait AeneasBlock extends PersistentNodeViewModifier with Block[PublicKey25519Proposition, SimpleBoxTransaction]{
  def number:Long
}

