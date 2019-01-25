package scorex.core.serialization

import io.circe.Json

/**
  * @Author is Alex Syrotenko (@flystyle) 
  *         Created on 23.03.18.
  */
trait JsonSerializable {
   type M >: this.type <: JsonSerializable

   def json : Json
   def serializer : Serializer[M]
}
