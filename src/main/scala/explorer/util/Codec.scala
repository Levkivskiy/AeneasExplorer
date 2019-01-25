package explorer.util

import block.PowBlock
import explorer.WsResponse
import scorex.util.encode.Base58

object Codec {

  implicit class ToReturnPowBlock(pb: PowBlock) {
    def toReturnPowBlock: WsResponse.ReturnPowBlock = {
      WsResponse.ReturnPowBlock(
        pb.number,
        Base58.encode(pb.id),
        Base58.encode(pb.parentId),
        pb.beneficiaryAddress.address,
        pb.timestamp,
        pb.nonce,
        Base58.encode(pb.merkleRoot),
        pb.transactionPool.map(b => b.uncodeJson.noSpaces)
      )
    }
  }
}
