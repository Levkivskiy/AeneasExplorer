package api.account

import java.time.{LocalDateTime, ZoneOffset}

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.testkit.{ImplicitSender, TestActorRef, TestKit, TestProbe}
import akka.util.Timeout
import api.util.Encryption
import history.storage.AeneasHistoryStorage
import history.{AeneasHistory, TempDbHelper}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.scalatest._
import scorex.core.utils.ScorexLogging
import scorex.util.encode.Base58
import scorex.crypto.hash.Sha256
import settings.AeneasSettings

import scala.concurrent.duration._
import scala.util.Success

/**
  * @author luger. Created on 11.05.18.
  * @version ${VERSION}
  */
class LoginActorTest(_system: ActorSystem)
  extends TestKit(_system)
    with ImplicitSender
    with FunSuiteLike
    with Matchers
    with BeforeAndAfterAll
    with ScorexLogging{

  lazy val settings = AeneasSettings.read()
  lazy val testFile = TempDbHelper.mkdir
  lazy val t = java.io.File.createTempFile("pre", "post")
  implicit lazy val timeout = Timeout(5.seconds)
  lazy val store = new LSMStore(testFile, maxJournalEntryCount = 200)
  val storage = new AeneasHistoryStorage(store, settings.miningSettings)
  var history = new AeneasHistory(storage, Seq(), settings.miningSettings)
  val seedPhrase = "aeneas"

  val privateId = Base58.encode(Sha256(seedPhrase.mkString(",").getBytes("UTF-8")))
  val testPublicSeed = Base58.encode(Sha256(privateId))
  val pwd = "test"

  override def beforeAll(): Unit = {
    super.beforeAll()
    val publicSeed = Base58.decode(testPublicSeed).get
    store.update(
      ByteArrayWrapper(publicSeed ++ String.valueOf(LocalDateTime.now().toEpochSecond(ZoneOffset.UTC)).getBytes()), Seq(),
      Seq (ByteArrayWrapper(publicSeed) -> ByteArrayWrapper(Encryption.encrypt(pwd, privateId).getBytes("UTF-8"))))
  }

  test ("testLogin" ) {
    //val test = TestProbe()

    val savedSeedsMessage = NewAccountEvents.SignIn(testPublicSeed, pwd)
    val scorexSettings = settings.scorexSettings.copy(
      dataDir = t, logDir = t)
    val actorRef = TestActorRef(new LoginActor(ActorRef.noSender, history, scorexSettings, store))
    val f = actorRef ? savedSeedsMessage
    val Success(r : NewAccountEvents.GeneratedSeed) = f.value.get
    log.info (s"r = $r")
    //r should be ()
    r.userKey.get.seed should be (testPublicSeed)
  }

  test ("testSavedSeeds") {
    val test = TestProbe()

    val savedSeedsMessage = NewAccountEvents.GetSavedSeeds()
    val scorexSettings = settings.scorexSettings.copy(
      dataDir = t, logDir = t)
    val actorRef = TestActorRef(new LoginActor(ActorRef.noSender, history, scorexSettings, store))
    val f = actorRef ? savedSeedsMessage
    val Success(r : NewAccountEvents.ReturnSavedSeeds) = f.value.get
    log.info (s"r = $r")
    //r should be ()
  }

  test ("testSeedWithAddress") {

  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    TempDbHelper.del(testFile)
  }
}
