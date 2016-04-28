package notebook.front

import notebook.JSBus._
import notebook.JsonCodec._
import notebook._
import play.api.libs.json._

trait IOSingleConnector[I, O] {
  @transient implicit def codec: Codec[JsValue, O]

  def toO: I => O

  val dataConnection: ValueConnection = JSBus.createConnection
  lazy val currentData: Connection[O] = dataConnection biMap codec

  def apply(newData: I) {
    currentData <-- Connection.just(toO(newData))
  }
}

trait SingleConnector[T] extends IOSingleConnector[T, T] {
  val toO: T => T = identity[T] _
}

trait SingleConnectedWidget[T] extends SingleConnector[T] with Widget with Serializable

trait IODataConnector[I, O] extends IOSingleConnector[Seq[I], Seq[O]] with Serializable {
  @transient implicit def singleCodec: Codec[JsValue, O]

  implicit lazy val codec = tSeq[O](singleCodec)

  def singleToO: I => O

  lazy val toO: Seq[I] => Seq[O] = (_: Seq[I]) map singleToO
}

trait DataConnector[T] extends IODataConnector[T, T] with Serializable {
  def singleToO: T => T = identity[T]

  override lazy val toO: Seq[T] => Seq[T] = identity[Seq[T]]
}

trait DataConnectedWidget[T] extends DataConnector[T] with Widget with Serializable
