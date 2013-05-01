# To Fix
* Set up a shell script to make temporary folders, parser excutable, etc.
* レイアウトが崩れる。JSの読み込みがおかしい可能性 addJSLibraries再考。

# ToDo

* Backup of the database
** JSON export
** Binary export
* Epub export with figures and refs
* Activity log
* Export of styled citation text (for writing a manuscript, slides, etc.)
* PDF Viewing
* More complete html parsing engine
** Especially for ACS journals


* StubとToDoを探すコマンド： find . -name "*.hs" | xargs egrep -i "(stub|todo)"
* Add log for fetching the data.
* Stop fetching images by default.
*   Or use images uploaded from the browser client.
* Add vim-like keyboard opearation
* Add functions to manage papers by the added date.
* Make a timeline view for adding papers.
* 階層のフォルダビューを左に表示できるようにする。フラットなタグビューとツリービューの切り替え
* フォルダ構成は既存のタグから自由に作れるようにする。複数のプロファイル
* を作って切り替えできるようにする。
* referencesが[[Reference]]なのを[Reference]にし、名前（1a,1bなど）から階層を作る、あるいは別の構造を保持することで階層を記憶する。実際問題階層を必要とする場面はあまりない。本文中からの引用を表示したい時くらい。
* abstractやmainhtmlのScriptタグを必ず削除する。XSS対策。
* 413 Too Largeの解消：例http://pubs.acs.org/doi/full/10.1021/cr3000994
