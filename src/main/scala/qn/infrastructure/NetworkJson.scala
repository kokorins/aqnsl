package qn.infrastructure

import play.api.libs.json._
import qn.Network

object NetworkJson {
  implicit val networkWrites = new Writes[Network] {
    override def writes(network: Network): JsValue = ???
  }
  implicit val networkReads = new Reads[Network] {
    override def reads(networkJson: JsValue): JsResult[Network] = ???
  }
}