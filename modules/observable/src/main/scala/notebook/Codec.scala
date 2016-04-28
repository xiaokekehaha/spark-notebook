package notebook

import java.text.DateFormat
import java.util.Date

import org.slf4j.LoggerFactory
import play.api.libs.json._

import scala.util.{Try, Success, Failure}

trait Codec[A, B] extends Serializable {
  @transient def encode(x: A): B

  @transient def decode(x: B): A
}

object JsonCodec extends Serializable {
  val log = LoggerFactory.getLogger(getClass)

  @transient implicit def formatToCodec[A](stringFallback:Option[String=>A]=None)(implicit f: Format[A]): Codec[JsValue, A] = new Codec[JsValue, A] {
    val stringFallbackRead = stringFallback map (f => Reads.of[String] map f)
    val reader = stringFallbackRead match {
        case Some(r) => f orElse r
        case None    => f
      }

    def decode(a: A): JsValue = f.writes(a)

    def encode(v: JsValue): A = reader.reads(v) match {
      case s: JsSuccess[A] => s.get
      case e: JsError => throw new RuntimeException("Errors: " + JsError.toFlatJson(e).toString())
    }
  }

  //implicit val ints:Codec[JsValue, Int] = formatToCodec(Format.of[Int])
  @transient implicit val longs: Codec[JsValue, Long] = formatToCodec(Some((_:String).toLong))(Format.of[Long])
  @transient implicit val doubles: Codec[JsValue, Double] =  formatToCodec(Some((_:String).toDouble))(Format.of[Double])
  @transient implicit val floats: Codec[JsValue, Float] = formatToCodec(Some((_:String).toFloat))(Format.of[Float])
  @transient implicit val strings: Codec[JsValue, String] = formatToCodec(Some(identity[String] _))(Format.of[String])
  @transient implicit val chars: Codec[JsValue, Char] = formatToCodec(Some((_:String).head))(Format(
    Reads.of[String].map(_.head),
    Writes((c: Char) => JsString(c.toString))
  ))
  @transient implicit val bools: Codec[JsValue, Boolean] = formatToCodec(Some((_:String).toBoolean))(Format.of[Boolean])

  @transient implicit def defaultDates(implicit d: DateFormat): Codec[JsValue, Date] = {
    new Codec[JsValue, Date] {

      def decode(t: Date): JsValue = JsNumber(t.getTime)

      def encode(v: JsValue): Date = v match {
        case JsString(s) => Try(s.toLong).toOption.map(new Date(_)).getOrElse(d.parse(s))
        case JsNumber(i) => new Date(i.toLong)
        case x => new Date(v.as[Long])
      }
    }
  }

  @transient implicit val ints = new Codec[JsValue, Int] {
    def decode(t: Int) = JsNumber(t)

    def encode(v: JsValue): Int = v match {
      case JsString(s) => s.toInt
      case JsNumber(i) => i.toInt
    }
  }

  @transient implicit val pair = new Codec[JsValue, (Double, Double)] {
    def decode(t: (Double, Double)): JsValue = Json.arr(t._1, t._2)

    def encode(v: JsValue) = {
      val Seq(JsNumber(x), JsNumber(y)) = v.asInstanceOf[JsArray].value
      (x.toDouble, y.toDouble)
    }
  }

  @transient implicit def tMap[T](implicit
    codec: Codec[JsValue, T]): Codec[JsValue, Map[String, T]] = new Codec[JsValue, Map[String, T]] {
    def encode(vs: JsValue): Map[String, T] = {
      val jo: JsObject = vs.asInstanceOf[JsObject]
      jo.value.map { case (name, jsValue) =>
        name -> codec.encode(jsValue)
      }.toMap
    }

    def decode(
      ts: Map[String, T]): JsValue = JsObject(ts.map { case (n, t) => (n, codec.decode(t)) }.toSeq)
  }

  @transient implicit def tSeq[T](implicit
    codec: Codec[JsValue, T]): Codec[JsValue, Seq[T]] = new Codec[JsValue, Seq[T]] {
    def encode(vs: JsValue) = vs.asInstanceOf[JsArray].value.map(codec.encode)

    def decode(ts: Seq[T]): JsValue = JsArray(ts.map(t => codec.decode(t)))
  }

  /*
  implicit def jdouble2jsvalueCodec[T](codec:Codec[JDouble, T]):Codec[JsValue, T] = new Codec[JsValue, T] {
    def encode(x:JsValue):T = codec.encode(x)
    def decode(x:T):JsValue = codec.decode(x)
  }*/

  @transient implicit def idCodec[T]: Codec[T, T] = new Codec[T, T] {
    def encode(x: T): T = x

    def decode(x: T): T = x
  }


}
