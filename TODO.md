# 不具合
* Bookmarkletから開かれるウィンドウが大きすぎる。
* 413 Too Largeの解消：例http://pubs.acs.org/doi/full/10.1021/cr3000994

# 機能追加

## 優先度高
* list UIの複数選択 
  * reparse
  * export citation
* Activity logの検索・フィルター機能
* Backup of the database
 * JSON export
 * Binary export

## 優先度中
* Import of database
 * Mendeley

## 優先度低
* Epub exportの完成度を高める。

* Export of styled citation text (for writing a manuscript, slides, etc.)

* PDF Viewing

* More complete html parsing engine

* PDFからのテクスト抽出。

* 論文を取得した時間をチャンクに分けることで、トピック分類する。（同じ時間帯に探した一連の論文は、同じテーマに関連する可能性が高い。）

* 研究者の論文リスト（Pubmed、あるいは一般のウェブページ）から総ての書誌情報をインポートする。

* ユーザー登録しないで使えるデモサイトの構築。フリーの論文を使う。PLoS, BMC Bioinformatics. ACS Author's choice

* Add log for fetching the data.

* 階層のフォルダビューを左に表示できるようにする。フラットなタグビューとツリービューの切り替え
* フォルダ構成は既存のタグから自由に作れるようにする。複数のプロファイルを作って切り替えできるようにする。

* referencesの階層構造の保持。
* referencesの本文からのジャンプの対応。


# 設計


* パーシングの構造
 * Structure definition by external file. That way, no compiling needed.
 * RPCなどを使った、別プロセスでのパーズ。キューを作って順次解析させる。
* PubmedのAPIの利用の拡充。referenceなども取れる。
  *その他のDBも検討。
    * PMCは全文が取れる。

* backgridjsを使って、/list のデータ管理をbackboneと組み合わせて使う。

* Mongoのレプリケーションを作る。

# その他メモ

* StubとToDoを探すコマンド： find . -name "*.hs" | xargs egrep -i "(stub|todo)"

