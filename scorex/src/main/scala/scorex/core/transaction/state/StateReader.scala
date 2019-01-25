package scorex.core.transaction.state

import scorex.core.VersionTag
import scorex.core.mainviews.NodeViewComponent

trait StateReader extends NodeViewComponent {

  //must be ID of last applied modifier
  def version: VersionTag

  def maxRollbackDepth: Int

}
