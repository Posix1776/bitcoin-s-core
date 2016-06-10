package org.bitcoins.core.consensus

import org.bitcoins.core.protocol.blockchain.MainNetChainParams
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.BitcoinSUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 5/24/16.
  */
class MerkleTest extends FlatSpec with MustMatchers {

  "Merkle" must "compute the merkle root for the genesis block" in {
    Merkle.computeBlockMerkleRoot(MainNetChainParams.genesisBlock).hex must be
    (BitcoinSUtil.flipEndianess("4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b"))
  }

  it must "correctly compute the merkle root for block 80,000 which has 2 transactions" in {
    //this block has 2 transactions in it which makes a nice symmetric merkle tree
    //https://blockchain.info/block-height/80000
    val coinbaseTx = Transaction("01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0704e6ed5b1b014effffffff0100f2052a01000000434104b68a50eaa0287eff855189f949c1c6e5f58b37c88231373d8a59809cbae83059cc6469d65c665ccfd1cfeb75c6e8e19413bba7fbff9bc762419a76d87b16086eac00000000")
    require(coinbaseTx.txId.hex == BitcoinSUtil.flipEndianess("c06fbab289f723c6261d3030ddb6be121f7d2508d77862bb1e484f5cd7f92b25"))
    val tx1 = Transaction("0100000001a6b97044d03da79c005b20ea9c0e1a6d9dc12d9f7b91a5911c9030a439eed8f5000000004948304502206e21798a42fae0e854281abd38bacd1aeed3ee3738d9e1446618c4571d1090db022100e2ac980643b0b82c0e88ffdfec6b64e3e6ba35e7ba5fdd7d5d6cc8d25c6b241501ffffffff0100f2052a010000001976a914404371705fa9bd789a2fcd52d2c580b65d35549d88ac00000000")
    require(tx1.txId.hex == BitcoinSUtil.flipEndianess("5a4ebf66822b0b2d56bd9dc64ece0bc38ee7844a23ff1d7320a88c5fdb2ad3e2"))

    val transactions = Seq(coinbaseTx,tx1)
    Merkle.computeMerkleRoot(transactions).hex must be ("8fb300e3fdb6f30a4c67233b997f99fdd518b968b9a3fd65857bfe78b2600719")
  }

  it must "correctly compute the merkle root for a block with 3 transactions" in {
    //https://blockchain.info/block-height/93500
    val coinbaseTx = Transaction("01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff08042a8b091b025e3cffffffff0100f2052a01000000434104d77816ded32ccc56fad6f455676c07908da96a37a7b9d2fd510cd4ddd92f3104f3d6e7134bd159fed3741522265a901d44ec2ab428231c0e4986c52a22f13577ac00000000")
    require(coinbaseTx.txId.hex == BitcoinSUtil.flipEndianess("a7c2b4a2cc940f9f541905048fe8352bd158dab18d15221fab7ee2187bd3cb5e"))
    val tx1 = Transaction("0100000006588e823839ae8e88dede39e38ff01e8c90d05a887425ed55eabecf6020738224000000008b483045022100f6db4e4e90a5760f69c19915483ba6a6315aa2e77d20e82352b695b6451230cd0220359f352a2b7d7a5a8ec5fb71ea58a03addb0340c0293444d553b4ef9c8b0810e0141047247fe4f3927ac892e569e70fd111f3f2ccff9038cbaa161055401713f32e6e2b7179114a2defdd4fbb705d0e2f590fb546386b05b86fe8894b8a6e74f2f5518ffffffff5f35f5725a031d43a50932beeeb863d82e064003ed0e9183afeafe0d5c1d7ffb000000008b48304502202347b11b43964b2e0acecb356a8f9d45d5a0dbbd6a916351bb221e4346fc3287022100a983c5f45e41e9dbe7ddd8490e070c1755205c3909ec7a2cabd8b96bdb8cfa1d0141047e848e792ae7d79b1ec2ce996e2679ae57e3a731db9d8f20c2be48d19c916d1db3e2b063a422e7511f60fe657134698528644935b3d2d75713e07e55f00487e7ffffffff5bf96d93cb82d645369761dc478ff36cf84691ea86c8a68255e7e52799e5aec4010000008a47304402205aa4bf439b85fc6ce12c933e05b387436456269acc4bef40fbce11e850c79f0b022033d425ac44ffb0ffe05708af025b3a4a19fb9e2127d6ede39dce8ff80e78f6270141045dedbe2d352e9327bfefd3dfcd808f87162277ddc4802c600110b736d20a7e6e263634079422a3e4cb00a769657dfa6718feb1cd9bfae1d4b152267fa2040d38ffffffffd9c85951982849492a422d2c918928d21e6c10b07ffaec2e566bea165ce6355d000000008b483045022100ec2eb7afd57c5b281d68708b0a46948221a9b0497736214ca9e5049bb2d764fe02201ddec4c71bdf73b3022b0c3f3e5a340126cefc2076c5d35edb849ee6006ed177014104060c334bdd808dc33008e27b3b01f8f0de9a074038e445fa3252a71a8cbf8b80abee043652b3ebf7cb1f921c2f6dd715858a8cc3c6fdde84f92dbaa9a91c0d51ffffffffba73f0deba362f932bebe223ac478303fd7b4a72dd45ac299882cf720f289eb2010000008b483045022100c0ecc02da03f9bcd1a29db63fd76f18025dd4f2a9de80818077106e093f6da6e0220714835dc93c86b77220564dbd9b9dc67855bfe6bf097670ad9b760b18bb3943c014104ebfe79ce9fe65ca2bca723b692039befba3df71cdd4dab7ad046b864644a5df70a4c90393549243bb475e7b5acdc08fc4b0307ae362861aa19f38e9b0f746c9affffffff056011d7b88b6ca196b0c953063c6c92d6dcb4d7ea31800da227971417ce68ec000000008b483045022100dbe5f0060b87ce7a63a6b46c7fcf19ab516313ff31f33470eecc456a1bb5f01d022027a1c2e0969fe98c458cd7c34e89c26de9ae769758ff5953751653469f94c375014104052cfb55b4a642b3453fbc9797ab7101623789b18f06b0091b03c03af9656a1ce4ac3a7bb65d634d023f9f7dde778a4dad6ae397761cc185de935e2959a0eedaffffffff0200969900020000001976a9141f6630103c210cc5be5c96ce04bac3ed0a9ad22188ac00093d00000000001976a91480d529f416e5fc5d98a8a24622abb45f0a03418888ac00000000")
    require(tx1.txId.hex == BitcoinSUtil.flipEndianess("1d74396699ae0effcd67fd5d031b780ff56c336bfc5d2d015d21db687d732764"))
    val tx2 = Transaction("0100000001eba8353ac2e5503f15548975108013246457ed83d331db760f0595b8bd7c54cb000000008c4930460221008c64f29882d9a59cbb070d75b4cdca56c04b523b0af37a0ffecee24e31cb2814022100b183ab317ad217f4a6f4e610c6138e5c2d7681d40f46201f268a5a90c1c07afa0141040b362c040204c13f6e1ec78b60978bdd76d851d4a1612cd9e82ead5177694f8f37fa4e8c78579876bbaf8a561772f320d3125f36cd1f1c5e9eb3f8bc08b626d2ffffffff0280e9fd97000000001976a914f0630fd41ff0722cf29de4db609f06a4c17fad2d88ac002a7515000000001976a9141dea9e37227b8d7a6296849fc76e00e8f5a6674e88ac00000000")
    require(tx2.txId.hex == BitcoinSUtil.flipEndianess("d8c9d6a13a7fb8236833b1e93d298f4626deeb78b2f1814aa9a779961c08ce39"))
    val transactions = Seq(coinbaseTx, tx1, tx2)

    Merkle.computeMerkleRoot(transactions).hex must be ("d277b5d20fab7cdb8140ab953323b585445d4920ad7226623d9c7ed0bc6b9a57")
  }

  it must "correctly compute the merkle root for the 100,000 block which has 4 transactions" in {
    //this block has 4 transactions in it which makes a nice symmetric merkle tree
    //https://blockchain.info/block-height/100000
    val coinbaseTx = Transaction("01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff08044c86041b020602ffffffff0100f2052a010000004341041b0e8c2567c12536aa13357b79a073dc4444acb83c4ec7a0e2f99dd7457516c5817242da796924ca4e99947d087fedf9ce467cb9f7c6287078f801df276fdf84ac00000000")
    val tx1 = Transaction("0100000001032e38e9c0a84c6046d687d10556dcacc41d275ec55fc00779ac88fdf357a187000000008c493046022100c352d3dd993a981beba4a63ad15c209275ca9470abfcd57da93b58e4eb5dce82022100840792bc1f456062819f15d33ee7055cf7b5ee1af1ebcc6028d9cdb1c3af7748014104f46db5e9d61a9dc27b8d64ad23e7383a4e6ca164593c2527c038c0857eb67ee8e825dca65046b82c9331586c82e0fd1f633f25f87c161bc6f8a630121df2b3d3ffffffff0200e32321000000001976a914c398efa9c392ba6013c5e04ee729755ef7f58b3288ac000fe208010000001976a914948c765a6914d43f2a7ac177da2c2f6b52de3d7c88ac00000000")
    val tx2 = Transaction("0100000001c33ebff2a709f13d9f9a7569ab16a32786af7d7e2de09265e41c61d078294ecf010000008a4730440220032d30df5ee6f57fa46cddb5eb8d0d9fe8de6b342d27942ae90a3231e0ba333e02203deee8060fdc70230a7f5b4ad7d7bc3e628cbe219a886b84269eaeb81e26b4fe014104ae31c31bf91278d99b8377a35bbce5b27d9fff15456839e919453fc7b3f721f0ba403ff96c9deeb680e5fd341c0fc3a7b90da4631ee39560639db462e9cb850fffffffff0240420f00000000001976a914b0dcbf97eabf4404e31d952477ce822dadbe7e1088acc060d211000000001976a9146b1281eec25ab4e1e0793ff4e08ab1abb3409cd988ac00000000")
    val tx3 = Transaction("01000000010b6072b386d4a773235237f64c1126ac3b240c84b917a3909ba1c43ded5f51f4000000008c493046022100bb1ad26df930a51cce110cf44f7a48c3c561fd977500b1ae5d6b6fd13d0b3f4a022100c5b42951acedff14abba2736fd574bdb465f3e6f8da12e2c5303954aca7f78f3014104a7135bfe824c97ecc01ec7d7e336185c81e2aa2c41ab175407c09484ce9694b44953fcb751206564a9c24dd094d42fdbfdd5aad3e063ce6af4cfaaea4ea14fbbffffffff0140420f00000000001976a91439aa3d569e06a1d7926dc4be1193c99bf2eb9ee088ac00000000")

    val transactions = Seq(coinbaseTx, tx1,tx2,tx3)

    Merkle.computeMerkleRoot(transactions).hex must be ("f3e94742aca4b5ef85488dc37c06c3282295ffec960994b2c0d5ac2a25a95766")

  }

  it must "correctly compute the merkle root for a block with 5 transactions" in {
    //https://blockchain.info/block-height/98500
    val coinbaseTx = Transaction("01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff07045359051b0113ffffffff0100f2052a01000000434104bca047b2df9d2e8afcd3000b4769caabd3ff5d668984cd4331fae4aa56b885f536a356252d65577445a1e25b0c9851a99c0ee4b89494c550d136813cbb597910ac00000000")
    require(coinbaseTx.txId.hex == BitcoinSUtil.flipEndianess("bde8a214516cb60a658df9515ba078d094ce416281f1277e83a42db8638e4d33"))
    val tx1 = Transaction("0100000001007320d46791b00322f9f9158cf480f8b623433776f4bd0a51f34db9281e3083010000008b483045022100c8893bf506b3b679ac384e02e6f7e4f94a5954b99ad4a88ee97b29de504d845b022037f20340a1c45edaefbfe25eb6c4a7b65e3369753fccabe510bbdee3a9064fd70141048d16442ee408075783d1293b515fe1b28ed39abb020c6199ce2f4a5c14d577fadc56cbacf8b5f9f75b900c3c3ba30d34f58b6aed5712bb0c5bd364985de3e26bffffffff0200d27b86080000001976a914e772268c12916cb35d6728f697e05af2ede3b46b88ac404b4c00000000001976a91483a2a9adc7e430b2bbf78864c22979b4cb69204288ac00000000")
    require(tx1.txId.hex == BitcoinSUtil.flipEndianess("96a446afdd9e0dbdd4d33cf14be18cb1bf8f46a8fd3f7fcbbad853b47d200c33"))
    val tx2 = Transaction("01000000020fee818ede67485682844b7611bf81abb26dd0aefce430259541761d0e52c74c000000008a473044022038521d9914da4e9c72cfbe0c90ba0adc73b429b84cd89155d3c5479a4e2772e30220172457e0e0796214a8ecd5b246b49d78bd1158e4b16943ae676f196a484c76e70141048431c896f2ad3bf1d32ddbd5116154f4dcd510ed5d27de529e0dd276a24a60e73b1999bb7b46c7edadc60da7cb5945094b5d37a9a83859a47dab5d2c59cd739effffffff7851dd60e5a5ea9bd1252d3128bd3b757ef1a744421799b76db34e0af70d4191010000008a4730440220373cce7182f148eb70ed523f4dd9007bd6f68a86f3284b8ec5588aa11d53455302205393f045b8134aeed54ac9081f9bb295250ca5c9863b7fb252ea8d7bb4cf661b0141041f2d46ae7a8a35bc3e09be573eeeabaae3608dd4f3bb2a75ec8a33ec127757f70230aadaae3ab1570f08e16969d88aee7a2addcf3bcf435b66cdd14b3a25303fffffffff0100e1f505000000001976a914e01f9eff99a725dd3c7548836477529c62c9139e88ac00000000")
    require(tx2.txId.hex == BitcoinSUtil.flipEndianess("d3561b5c19b8506f0e5230471ce379feecd5506f4ea4605a1c220652fe8ad0a6"))
    val tx3 = Transaction("0100000001d7dc7643bf3c9fe8bad464b9447916928d4cc5419d6a6e5d0b32ae25534d7f0a000000008a473044022003fc88222e4d52cd9753887145acf2f2a9f4cfb160eddfb3b27b82b10c1ac9b8022079fc38b0fc74a154aae9df854a3ebc4dc1d43ed6bf9cd2af3617ead8dc86d2f501410454f63db0081744756df83f492c642de26ddf2089ebf7ee3abfdcd7a4908dfb841da3c602302ce2445022ed9c6146c5c784ce2d3cb9b3a393dc9311f5ea84a268ffffffff0180841e00000000001976a91439aa3d569e06a1d7926dc4be1193c99bf2eb9ee088ac00000000")
    require(tx3.txId.hex == BitcoinSUtil.flipEndianess("52eedbfd8596f629d61cadb0cddf781d1f64fbb5ea5f015db760a4bf2e603568"))
    val tx4 = Transaction("0100000001330c207db453d8bacb7f3ffda8468fbfb18ce14bf13cd3d4bd0d9eddaf46a496000000008c493046022100cf5c76c34de0fc8538f1dc8663edac41c05e4fc42fb2f6188c6a044c6906c83b022100a96aca4ebdae7502c0d2241ac4b72bf6e5a56debcc251cfe7380ecabf64a742e014104e919396bf90935fba5f6d58af2f4e844a65a239ff23b695c982345a36bb7dbc23f4a950fc5092ff8c1a64209ad147a82b446defe9480970fb36990e8cceb6da3ffffffff02404b4c00000000001976a914004c7a02611b21082d41d6520042d038a183c51288acc0862f86080000001976a9143aa3e0f999ac63971bd568ae4336cedeba9682b788ac00000000")
    require(tx4.txId.hex == BitcoinSUtil.flipEndianess("b79606d7377285e1a4f9cb757c9f1a1d36f09c10bc52be49b170c6dd40767669"))

    val transactions = Seq(coinbaseTx,tx1,tx2,tx3,tx4)
    Merkle.computeMerkleRoot(transactions).hex must be ("36b38854f9adf76b4646ab2c0f949846408cfab2c045f110d01f84f4122c5add")
  }

  it must "correctly compute the merkle root for a block with 11 transactions" in {
    //https://blockchain.info/block-height/101000
    val coinbaseTx = Transaction("01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0704cb04041b014affffffff0100f2052a01000000434104204b65fb53be30419ac44ba3a73f128bb9fac510e602de1442ac989a44dcd5a3b9229b594ec437e7ee5d3ffc7e858add6c3f4787b8a8c3d6b4f8b091bf823c3dac00000000")
    require(coinbaseTx.txId.hex == BitcoinSUtil.flipEndianess("a2c90afe34f1a9b975d29e8f0d80b04acfa51d775160b11cf2dd7221008fc3c8"))
    val tx1 = Transaction("0100000001a33c65f483575784ca237716c0b9a51a3262c4ee8454ac88352f1bd08e7c71ad0000000049483045022046957e1ad5ddbae7342bbc270cec6f08a5c82211bd06e2b19d140ebfdc708692022100b5289e1d23b48b6975d9b9d1418616f1fa570ac4e10946bc4e7b8baf73bbbca401ffffffff0240147422010000001976a914e2757955eec59afd698d2f3085e48ad98ab1bc8b88acc0dd9107000000001976a914b55dd29fed068ccc1bc17312559953f6583ee7e488ac00000000")
    require(tx1.txId.hex == BitcoinSUtil.flipEndianess("e708cd2450a95173a8af0a7e256010dcbfa3895a467a440f0bb78af4ce69a774"))
    val tx2 = Transaction("0100000001844cecc7af8ebbaf190a2c9483b6c13396ba004ec078e6d53c9729826bcb332100000000494830450220757a3bd57a66a9f46a0e7e71a4a8b2aee6d73b39285a16ff70c5e41469942b5b022100b55db0e6c38ed9bdd696a57a5ef1685bf536203eb87e59453b43446c0388a5a201ffffffff0280568322010000001976a914bb0667115ba371dda3abd3c6f6fee2777cb4336188ac809b8207000000001976a91463d4dd1b29d95ed601512b487bfc1c49d84d057988ac00000000")
    require(tx2.txId.hex == BitcoinSUtil.flipEndianess("7cb7f0c5bad1fc13748e788410f0020db92ba700ad2a668d9c2b6c4aabda2c85"))
    val tx3 = Transaction("0100000001521f1014e1439a4a979b96543eb9fde6fed3033c041777ac888e998a9b5ad687000000008b4830450220042cc7cd451b9d8ac058657b6ea2cce47748a0e4d121fe8959bb12e1d7a33343022100c001bb864c8dd2202c4700a65e91cee7e6b0b75a6075a8e174042e2742c80fe30141046b6331d6d3178e478e88487cf2130365e8c9a099eec187750f98cd6765c0d8f6863ef951072a46bb189985d1406cae2bebe656eeadbc4a0c734a6c462ec35df6ffffffff0280da2d09000000001976a914c22420641cea028c9e06c4d9104c1646f8b1769088ac80a61117010000001976a9142db035e0550aae55ba9275769a487d9fd06da81188ac00000000")
    require(tx3.txId.hex == BitcoinSUtil.flipEndianess("5a8b536a80285454839571baf006c9346ef8e5e82dc27d011d307f4c23c26dae"))
    val tx4 = Transaction("010000000123e1398b14f1cba6ddc52214345710c9349e0081afa5ab7a95d6511c2aceec0d010000008c493046022100c2fa6176889034e1f240174808e58302501970424a3dc7ecb127a0362d9716e002210093010e4e06e53efeaa0adf437fc068fcaac3615dd12b0ff6a7eb583c0390ef5f01410434bc0b5bed684ac2baf2c1112531074ae65fe60633514a413eb610862e6e25f562bc75f68e5b3590a2aaed3aa1cb9493ae5a1df448c219c1029f92ea6589dfe1ffffffff02c02f720b010000001976a914e5d802968edc0dab6fc68f91e9b49681854bd10788ac40bc120a000000001976a914c928507652fe7f42e838566153357d81c27b35ff88ac00000000")
    require(tx4.txId.hex == BitcoinSUtil.flipEndianess("29ae963299c9531fce18750fe64862f032a5a40f15343092b4c432eb231a4084"))
    val tx5 = Transaction("0100000001932c744f53b90a6a4179c4ff98902674fc8b306b476d6e9380b2b370cd79378c010000008b483045022070058478ae92aa03b96f3fcf6498602aa08f14bf467ae306e91515602dd9ae3e022100e7b2946a281f68719f4eb4e46609e4c6f560d0f074dd3df49303fed645afccc3014104b85cded1eb71e8a4ee3b35beff0642d750e0cd9c656679b3b38b61bec3aa785649c742aaa771787a952096ce2b937db0370422ed3d6cfb649616d269b0ca6afaffffffff02008c7d0a000000001976a91439a17a80338f95445d123e8e9bde1ca805e06bfd88ac80be03ef000000001976a914c87ab952fd6e53493215394ec6aea2ae36ed193188ac00000000")
    require(tx5.txId.hex == BitcoinSUtil.flipEndianess("c55397fa43afe8a75adcf4f0f51a962cc550fa14952528c1dbb11e92bc4cd3fd"))
    val tx6 = Transaction("010000000193e57d0a24ff217fca2ea80192285b9e954f72d916b0f1417cb99638ad3becb0000000008b483045022100e8cb0654d721a3f5ca0bedfd6dce9077f5b827df7b43434f3b94c95d5b06d617022040c99a129fffcb4779b9dbc5d9d8804e10cdbed910d1d1d66c4ccefa74792ec4014104fb3d1c8a73dbc508def81d7cb139d5dd2206d0373d4c37a8c2c6fa02bfc5adf8128c68a776823cc0956e783b7465315237c4382364fb37337561e502c75ae1b4ffffffff020001970d000000001976a914a8772cf424b59eb26e8ab71c37266264ccd9893b88acf1beccc9000000001976a914926fd6a8d607e8be59b19e4f8b2d98a818841bf288ac00000000")
    require(tx6.txId.hex == BitcoinSUtil.flipEndianess("28b8d356e236cc001a2d0e6a8468a290e2b7a8693c246c78c8a30dc0ba16016d"))
    val tx7 = Transaction("0100000001c839d2518dde2a3916b4a0bc982a0d2f8b282ce010cf68fc2ff04bfc71a0bbb5010000008b483045022100f3c89528d32017e940ce5081214be1836baab972e0b13bfc486091766cce50020220530507644fa99695d6671fbd26605ea876e2afe73fa8413382348948ba20d60f014104602e552211f29dc5b9eee5b679fb08d21ba14154071d7d8cda23dbdd5c6c8e1656a88a0c2ac4786a4b58def140ea016675e31a09e87d355f18296b658de828ecffffffff02431cff3d000000001976a914d86754c15540fbb0f9d131b041581afe9cc65d6988ac4013b920000000001976a914fcff8f28c7f2dce1edf93e8795cd1d201e2a9c8688ac00000000")
    require(tx7.txId.hex == BitcoinSUtil.flipEndianess("49d72c911b717a7e922ede3241a0bd4f6327dd14cd3a8cba2a11beea3e77238f"))
    val tx8 = Transaction("0100000001010114fdd45e7b99f6f5caea21645c2693d0609ba580937686ba21f3235978cb000000008a4730440220116bc3143a675d972a78c3d67aa44b58305ecd94bcba286d8275077772a895bd02206c6c8760ffba11147d98517bd4299db0b11b749cb0b2aa992aa9d03a2a41ba5a014104b515bf108c71b23aeae6b3b61fce7d4398e7c117463f2071aeb8f98923058e438c4e2c7a6dfdfcb6bdde622796aa2d78df16d044170802cf26b10938f3bdffe5ffffffff02403cf18a020000001976a914db8c008ab5e1d2cb9bf777eea76f29b7828c6c1388ac404b4c00000000001976a9140c19e5b68422628d748a995a943e751f53da8e9e88ac00000000")
    require(tx8.txId.hex == BitcoinSUtil.flipEndianess("cae533bd524a8c0ef25a09ed0608b0f1e6d2233e2128263c2a11400698334626"))
    val tx9 = Transaction("010000000156c375d64acd0b2c0e5757046c426aa5569e2da805ee3d3b72a136ddbbe770af010000008b483045022037599894a6721378942104ab871c595262fcf61188408609606353129de844e3022100af4104a2695a7ac3c6534ffff54d328ea510a08f2ef331e68e44703a59e9d55c014104953bf42be63bb3b086716bcf04d234cd5a1548ef8dcab6a85277fb163cab71dc98860d807be53dc5aa6094f401508ebb9b60ebe40d898d058f0812f7b84f65f7ffffffff0280841e00000000001976a9149fb2b87cf1dcdd22de8036fa9c449a6b242bb16c88ac80841e00000000001976a914af501f804a4ad6177b77fb64f22e60e2534b0f4488ac00000000")
    require(tx9.txId.hex == BitcoinSUtil.flipEndianess("9798e1af941e9fc24bfb2af7ae6e00510c2546c95606d9452ac333252940fc79"))
    val tx10 = Transaction("010000000409427b1f9d55d558f47fb865796ba455a18fb19d8afe7ac802c2df6bb9c4bdea000000008b483045022100d75a7fb24c3b94ed1be19922286ca62aa618291a09e840efcab5f7ac2d71e91f02204578e6c60e69b60179d45359cb29d26a3b8a8148fc658b0a9e1ab28bdf3939ad014104de68dcf94795f2ea428828430381594ad49126b36ffbaf055bfe7125b8d2abfe36bd3f82808f4317033396efdc3b64804f7820636de87f5229552eed8e40ada7ffffffff9e8357c7dec0c0f3c1ef86e79d454ceeefd4c25556b77b66c46e41f23d9f7470000000008a47304402202856dee4c597a9bb786e7a26099375ca3c937f84c9e465cf875ce20ee21710360220575fb1294cf901f35d636f81bfac37b3b285011359a109928227f72388f3d967014104045fbbbd37f38fcdc1d6ba91ec1270b2c4cae9ecafd6fbd7850729a68a1bbde6844e7430c4a7d2a397c1719abf0b66c94abef34381ed435521847cecd8c67b66ffffffff2b2e8b86a0a7d2c68686d95f69d741689502242e3d198257da3e56723ff4a8bd000000008b483045022100cdd5ddedd3204c5f4d8e3eb43e7b3831f60be40fbd989616314159f26e49754b022048aad835a7b11235b59e24fbe70b162d0f5fadf11e800e432b96e0c526c23d5301410426e046799b2fe0c88d8fe6a6edfb147843abead2cb6c0786fd6f1173946df50502708d2e01e24da7f4bc3f65bd0dd7101eece8dbd3e0adbc2be0e96bf52f7c5bffffffff690b213d81673522ade46cce250f446bb1dd9307911a088017d7a81d9b165a86000000008c493046022100d2d55c279bccf504bba239e75901bbeda5556d651ec7753a1ce43f2d74929e5f022100ee8134caac18f3862a5e3c048829dec9323b33bcd86235b115dc909984891db801410488da06bc75b9ea5539f54d5f22568c239a9c0845bf948f156e05096d4584a023bf0504b737f1de61fb4dcbfa58fed558ffed50322ff8578d4b9e3ce05905797effffffff01c0cf6a00000000001976a914ea2ba5d2ecaf296c163fa0eacb35b9d53d8e6aac88ac00000000")
    require(tx10.txId.hex == BitcoinSUtil.flipEndianess("f7a894ce552d9ecb0deb2d8bdd1ee22d5e641aa4c382685a41e9f28c5269a96e"))

    val transactions = Seq(coinbaseTx,tx1,tx2,tx3,tx4,tx5,tx6,tx7,tx8,tx9,tx10)

    Merkle.computeMerkleRoot(transactions).hex must be ("2c8230dfb6c7ad0949cbdb7d3c3616cc10770db7e832bbf5c9b261388828c6c4")
  }
}
