package validators

import block.{AeneasBlock, PowBlock}
import history.storage.AeneasHistoryStorage
import scorex.core.block.BlockValidator
import scorex.core.utils.ScorexLogging
import scorex.util.encode.Base58
import settings.SimpleMiningSettings

import scala.util.Try
;

/**
 * @author is Alex Syrotenko (@flystyle)
 * Created on 25.01.18.
 */
class DifficultyValidator(settings: SimpleMiningSettings, storage: AeneasHistoryStorage)
				extends BlockValidator[AeneasBlock] with ScorexLogging {

   override def validate(block: AeneasBlock): Try[Unit] = Try {
      require(block match {
         case b: PowBlock =>
            b.correctWork(storage.getPoWDifficulty(None), settings)
      },
      s"Work done is incorrect for block ${Base58.encode(block.id)} " +
      s"and difficulty ${storage.getPoWDifficulty(Some(block.parentId))}")
   }

}

object DifficultyValidator extends ScorexLogging {
   def correctWork(b : AeneasBlock, settings: SimpleMiningSettings, storage : AeneasHistoryStorage) : Boolean = {
      val target = settings.MaxTarget / storage.getPoWDifficulty(Some(b.parentId))
      val dig = BigInt(1, b.id)
      log.info(s"Difficulty Validator diff computed: ${target-dig} and validation result is is ${dig < target}")
      dig < target
   }
}
