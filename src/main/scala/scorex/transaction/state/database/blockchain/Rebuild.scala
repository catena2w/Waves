package scorex.transaction.state.database.blockchain

import java.io.File

import com.typesafe.config.ConfigFactory
import org.h2.mvstore.MVStore
import scorex.account.PrivateKeyAccount
import scorex.api.http.BlocksApiRoute
import scorex.app.ApplicationVersion
import scorex.block.Block
import scorex.consensus.ConsensusModule
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.crypto.encode.Base58
import scorex.network.TransactionalMessagesRepo
import scorex.transaction.{SimpleTransactionModule, Transaction}
import scorex.waves.consensus.WavesConsensusModule
import scorex.waves.settings.WavesSettings
import scorex.waves.transaction.WavesTransactionModule

import scala.reflect.runtime.universe._
import scala.util.Failure


class Rebuild(val settingsFilename: String) extends scorex.app.Application {
  override val applicationName = "waves"
  private val appConf = ConfigFactory.load().getConfig("app")
  override val appVersion = {
    val raw = appConf.getString("version")
    val parts = raw.split("\\.")
    ApplicationVersion(parts(0).toInt, parts(1).toInt, parts(2).split("-").head.toInt)
  }

  override lazy val apiRoutes = Seq(BlocksApiRoute(this))
  override lazy val apiTypes = Seq(typeOf[BlocksApiRoute])


  override implicit lazy val settings = new WavesSettings(settingsFilename)
  override implicit lazy val consensusModule = new WavesConsensusModule()
  override implicit lazy val transactionModule: SimpleTransactionModule = new WavesTransactionModule()(settings, this)


  def rebuild() = {
    val version = 1: Byte
    val folder = "/tmp/scorex/waves/data/"
    new File(folder).mkdirs()
    val newBlockchainFilename = folder + "bnew.dat"
    new File(newBlockchainFilename).delete()
    val oldBlockchain = transactionModule.blockStorage.history.asInstanceOf[StoredBlockchain]
    val newStorage = new MVStore.Builder().fileName(newBlockchainFilename).compress().open()
    val newBlockchain = new StoredBlockchain(newStorage)
    val newState = new StoredState(newStorage)
    val transactionsToExclude: Seq[String] =
      Seq("5mB6Yka7atT7PjzV129tkmx3VCxRdNCBTvY8LuLBgWEfaFfATKBX2qZWL9YqszTjArxBWnQDw1PE5WYvGxXEuvPe")
    val accountSeeds: Seq[String] = Seq("9y3B5caxAGbAZNnqYLt7JzzphbsRokhV5kVUVajzf6j7")
    val accounts: Map[String, PrivateKeyAccount] = accountSeeds.map { seed =>
      new PrivateKeyAccount(Base58.decode(seed).get)
    }.map(p => p.address -> p).toMap


    var ref: Array[Byte] = oldBlockchain.blockAt(1).get.referenceField.value
    newBlockchain.appendBlock(oldBlockchain.blockAt(1).get)
    newState.processBlock(oldBlockchain.blockAt(1).get)
    ref = newBlockchain.lastBlock.uniqueId

    require(oldBlockchain.height() > 1)

    (2 to oldBlockchain.height()) foreach { height =>
      val oldBlock = oldBlockchain.blockAt(height).get
      val transactions: Seq[Transaction] = oldBlock.transactions
        .filter(tx => !transactionsToExclude.contains(Base58.encode(tx.signature)))
      val account: PrivateKeyAccount = accounts(oldBlock.signerDataField.value.generator.address)
      implicit val cm: ConsensusModule[NxtLikeConsensusBlockData] = consensusModule

      val newBlock = Block.buildAndSign(version,
        oldBlock.timestampField.value,
        ref,
        oldBlock.consensusDataField.value.asInstanceOf[NxtLikeConsensusBlockData],
        transactions,
        account)(cm, transactionModule)

      newBlockchain.appendBlock(newBlock) match {
        case Failure(e) => throw e
        case _ =>
      }
      require(newState.processBlock(newBlock).isSuccess)
      ref = newBlock.uniqueId
      newStorage.commit()
    }
    println(s"Blockchain recovered with ${oldBlockchain.height()}!")
  }

  //checks
  require(transactionModule.balancesSupport)
  require(transactionModule.accountWatchingSupport)
  override lazy val additionalMessageSpecs = TransactionalMessagesRepo.specs
}

object Rebuild extends App {
  val filename = args.headOption.getOrElse("settings-local1.json")

  val application = new Rebuild(filename)
  application.rebuild()


}
