package scorex.transaction.state.database.blockchain

import java.io.File

import akka.actor.Props
import com.typesafe.config.ConfigFactory
import org.h2.mvstore.MVStore
import scorex.account.PrivateKeyAccount
import scorex.api.http._
import scorex.app.ApplicationVersion
import scorex.block.Block
import scorex.consensus.ConsensusModule
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.consensus.nxt.api.http.NxtConsensusApiRoute
import scorex.crypto.encode.Base58
import scorex.network.{UnconfirmedPoolSynchronizer, TransactionalMessagesRepo}
import scorex.transaction.{SimpleTransactionModule, Transaction}
import scorex.waves.consensus.WavesConsensusModule
import scorex.waves.http.{WavesApiRoute, DebugApiRoute, ScorexApiRoute}
import scorex.waves.settings.WavesSettings
import scorex.waves.transaction.WavesTransactionModule

import scala.reflect.runtime

class Rebuild(val settingsFilename: String) extends {
  override val applicationName = "waves"
  private val appConf = ConfigFactory.load().getConfig("app")
  override val appVersion = {
    val raw = appConf.getString("version")
    val parts = raw.split("\\.")
    ApplicationVersion(parts(0).toInt, parts(1).toInt, parts(2).split("-").head.toInt)
  }

} with scorex.app.Application {

  override val apiRoutes = Seq.empty
  override val apiTypes = Seq.empty
  override implicit lazy val settings = new WavesSettings(settingsFilename)
  override implicit lazy val consensusModule = new WavesConsensusModule()
  override implicit lazy val transactionModule: SimpleTransactionModule = new WavesTransactionModule()(settings, this)

  val version = 1: Byte
  val folder = "/tmp/waves/"
  new File(folder).mkdirs()
  val oldBlockchainFilename = folder + "bold.dat"
  val newBlockchainFilename = folder + "bnew.dat"

  def rebuild() = {
    val oldBlockchain = new StoredBlockchain(new MVStore.Builder().fileName(oldBlockchainFilename).compress().open())
    val newStorage = new MVStore.Builder().fileName(newBlockchainFilename).compress().open()
    val newBlockchain = new StoredBlockchain(newStorage)
    val newState = new StoredState(newStorage)
    val transactionsToExclude: Seq[String] = Seq.empty
    //map from account address to private key account
    val accounts: Map[String, PrivateKeyAccount] = Map.empty


    var ref: Array[Byte] = Block.genesis(settings.genesisTimestamp).uniqueId
    (1 to oldBlockchain.height()) foreach { height =>
      val oldBlock = oldBlockchain.blockAt(height).get
      val transactions: Seq[Transaction] = oldBlock.transactions
        .filter(tx => !transactionsToExclude.contains(Base58.encode(tx.signature)))
      val account: PrivateKeyAccount = accounts(oldBlock.signerDataField.value.generator.address)
      implicit val cm: ConsensusModule[NxtLikeConsensusBlockData] = consensusModule
      val newBlock = Block.buildAndSign(version,
        oldBlock.timestampField.value,
        ref,
        oldBlock.consensusDataField.asInstanceOf[NxtLikeConsensusBlockData],
        transactions,
        account)(cm, transactionModule)

      ref = newBlock.uniqueId
      newBlockchain.appendBlock(newBlock)
      newState.processBlock(newBlock)
      newStorage.commit()
    }

  }

  //checks
  require(transactionModule.balancesSupport)
  require(transactionModule.accountWatchingSupport)
  override lazy val additionalMessageSpecs = TransactionalMessagesRepo.specs
}

object Rebuild extends App {
  val filename = args.headOption.getOrElse("settings.json")

  val application = new Rebuild(filename)
  application.rebuild()


}
