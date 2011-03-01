# Cacoo Mode

Cacoo (http://cacoo.com) で書いた画像を高速に検索したり、バッファの中でインラインで表示したり、編集画面をさっと開くことのできるマイナーモードです。画像はローカルにキャッシュしていますので、オフラインでも画像を見ることができます。インライン表示時には自動的に指定のサイズに縮小します。

実際には Cacoo だけではなく、URLで参照できて Emacs と ImageMagick が解釈できる図であれば大抵インライン表示できます。

良く似たものに iimage（Emacs標準添付）があります。 iimage はローカルのファイルを表示するのみですが、それを Web から取得可能にしたり、自動で縮小したり、その他 Cacoo の編集機能をつけたようなものです。

## インストール

必要なもの、環境
- anything.el
- deferred.el, concurrent.el
  - https://github.com/kiwanami/emacs-deferred/raw/master/deferred.el
  - https://github.com/kiwanami/emacs-deferred/raw/master/concurrent.el
- wget (設定によってcurlなどでも可)
- ImageMagick (convert, identify, display)

ロードパスに cacoo.el を置き、以下のように呼び出し用のコードを .emacsなどに追加してください。以下の例では、Altキーを押しながら「-」を押すとOn/Offが切り替わるようになります。

設定例:

    (require 'cacoo)
    (setq cacoo:api-key "APIKEY") ; option
    (global-set-key (kbd "M--") 'toggle-cacoo-minor-mode)

'cacoo:api-key' は、Cacooにログインして、「設定」＞「APIキー」のページから取得できる文字列です。
cacoo-mode 起動時に API で Cacoo に接続して、自分が作成した図の一覧を取得します。

## 使い方

テキスト中に以下のように記述して 'cacoo:reload-all-diagrams-command'(C-c , R) すると Cacoo の絵が入ります。

    [img:https://cacoo.com/diagrams/6m4ATG1ddlUiHPqd-0FAF7.png]

'cacoo:anything-command'(C-c , I) すると、Cacooの絵（各シートも含めて）をAnythingで選ぶ画面になります。（ちょっと遅れてプレビュー画像が出ます。）

貼り付けた図はローカルにキャッシュされます。キャッシュは現在のディレクトリ（バッファが保存されるディレクトリ）のなかの「.cimg」というディレクトリに保存されます。オリジナルの図とリサイズされた図が保存されます。

キャッシュがあればネットワークに接続にいきません。リロード 'cacoo:reload-all-diagrams-command' するとキャッシュを消して取りに行きます。リサイズもこのときに行われるので、サイズを変えたいときはリロードしてください。

## キーバインド

'cacoo-minor-mode' が On の時に以下のキーバインドが使えます。使いやすく 'cacoo-minor-mode-keymap' をカスタマイズしてみてください。

### バッファ全体に対して
- C-c , T : バッファのすべての図をテキストに戻す
- C-c , D : バッファのすべての図を表示する（キャッシュがあればネットワークに接続しない）
- C-c , R : バッファのすべての図を取得し直す

### カーソール直後の図に対して
- C-c , t : テキストに戻す
- C-c , d : 図を表示する（キャッシュがあればネットワークに接続しない）
- C-c , r : 図を取得し直して表示する
- C-c , e : 図の編集画面を表示する（https://cacoo.com/diagrams/xxxxx/edit）
- C-c , v : 図の生涯画面を表示する（https://cacoo.com/diagrams/xxxxx）
- C-c , V : ローカルの図を外部ビューアーで開く

### Cacooの機能に対して
- C-c , I : Anythingで図を選択して挿入
- C-c , N : 新規図の作成 （https://cacoo.com/diagrams/new）
- C-c , l : 図の一覧 （https://cacoo.com/diagrams/）

### ナビゲーション、編集
- C-c , n : 次の図に移動
- C-c , p : 前の図に移動
- C-c , i : 図のマークアップを挿入
- C-c , y : クリップボードのテキストを使って図のマークアップを挿入

### その他
- C-c , C : キャッシュディレクトリを空にする

## 表示できる図について

Cacoo 以外でも以下のような図を表示することができます。（もちろん編集はできません）

- Web上の画像
  - [img:http://example.com/zzz.png]
- ローカルの画像（絶対パス）
  - [img:file:///xxx/yyy/zzz.png]
- ローカルの画像（相対パス）
  - [img:zzz.png]

画像の取得や変換中にエラーが起きた場合、該当箇所の色が変わります。また、マウスオーバーで短くエラーメッセージがポップアップで表示されます。大抵の問題は、 wget が見つからない、 convert が見つからない、URL先の図が見つからないなどだと思います。

## カスタマイズなど

ブラウザは 'cacoo:browser-function' で設定してあるものを使います。デフォルトは 'browse-url-browser-function' の値です。

例：Macでsafariを使う場合

    (setq cacoo:browser-function 'browse-url-generic)
    (setq browse-url-generic-program "open")

'cacoo:img-regexp', 'cacoo:img-pattern' を変更することで、Wiki記法などにあわせることができます。'cacoo:img-regexp' は正規表現文字列のリストにもできますので、いくつかの記法を登録しておくことが出来ます。

例：複数登録や貼り付け文字列のカスタマイズ

    (setq cacoo:img-regexp 
         '("\\[img:\\(.*\\)\\][^]\n\r]*$"
           "\\[f:\\(.*\\)\\][^]\n\t]*$"
           "<img src=[\"']\\(.*\\)[\"'][ ]*\\/>[^\n\t]*$"
           ))
    (setq cacoo:img-pattern "<img src=\"%s\" />")

キャッシュ用のディレクトリ名は 'cacoo:img-dir' で指定します。いちいちディレクトリ作成で確認が必要なければ、 'cacoo:img-dir-ok' を t に設定してください。

画像の縮小サイズは 'cacoo:max-size' で指定します。以下のようにスペース区切りで書くことで画像ごとにも指定できます。

    [img:file:///xxx/yyy/zzz.png 600]

外部画像ビューアーのプログラムは 'cacoo:external-viewer' で指定します。nilに設定するとEmacsで開こうとします。

Emacs22などではPNGの透過部分が黒くなることがあります。 '(setq cacoo:png-background "white")' を設定に追加して画像のリロードをしてみてください。

## プラグイン

プラグインによって、その場で画像を生成したり高度なこともできるようになっています。

UMLやシーケンス図のほかにも、LaTeX数式や、GoogleChartAPI のようなグラフ、また gnuplot や graphviz のようにコマンドラインから図を生成するようなものも本文中に書けるようになりました。

詳しくはプラグイン周辺のソースの中のコメントを参照してください。


## ライセンスなど

- License:  GPL v3
- Repositor:  http://github.com/kiwanami/emacs-cacoo
- SAKURAI, Masashi (m.sakurai atmark kiwanami.net)
