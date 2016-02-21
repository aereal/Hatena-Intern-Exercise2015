package hatena.intern

import hatena.intern.helper._

class Exercise2Spec extends UnitSpec {

  describe("LTSV Parser") {
    it("LTSVファイルが正しくパースされていること") {
      val logs = LtsvParser.parse("/Users/aereal/devel/src/github.com/hatena/Hatena-Intern-Exercise2015/sample_data/log.ltsv") // リポジトリ内の`sample_data/log.ltsv`へのパスを指定してください
      logs.size shouldBe 5

      val logsList = logs.toList
      val firstLog = logsList(0)
      firstLog.host shouldBe "127.0.0.1"
      firstLog.user shouldBe "frank"
      firstLog.referer shouldBe "http://www.hatena.ne.jp/"
    }

    it("LTSVファイルが正しくパースできない形式の場合") {
      // エラーハンドリングの設計を考えながら、テストを書いてみてください
    }

    it("LTSVファイルが存在しない場合") {
      // エラーハンドリングの設計を考えながら、テストを書いてみてください
    }
  }

}
