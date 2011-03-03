# Cacoo Mode

Cacoo (http://cacoo.com) で書いた画像を高速に検索したり、バッファの中でインラインで表示したり、編集画面をさっと開くことのできるマイナーモードです。画像はローカルにキャッシュしていますので、オフラインでも画像を見ることができます。インライン表示時には自動的に指定のサイズに縮小します。

実際には Cacoo だけではなく、URLで参照できて Emacs と ImageMagick が解釈できる図であれば大抵インライン表示できます。

良く似たものに iimage（Emacs標準添付）があります。 iimage はローカルのファイルを表示するのみですが、それを Web から取得可能にしたり、自動で縮小したり、その他 Cacoo の編集機能をつけたようなものです。

## インストール

### 必要なもの、環境

- cacoo.el （本体）
  - https://github.com/kiwanami/emacs-cacoo/raw/master/cacoo.el
  - https://github.com/kiwanami/emacs-cacoo/raw/master/cacoo-plugins.el
- deferred.el, concurrent.el
  - https://github.com/kiwanami/emacs-deferred/raw/master/deferred.el
  - https://github.com/kiwanami/emacs-deferred/raw/master/concurrent.el
- anything.el
  - http://www.emacswiki.org/emacs/Anything 
- wget (設定によってcurlなどでも可)
- ImageMagick (convert, identify, display)

ロードパスに cacoo.el などを置き、以下のように呼び出し用のコードを .emacsなどに追加してください。以下の例では、Altキーを押しながら「-」を押すとOn/Offが切り替わるようになります。

設定例:

    (require 'cacoo)
    (require 'cacoo-plugins)      ; option
    (setq cacoo:api-key "APIKEY") ; option
    (global-set-key (kbd "M--") 'toggle-cacoo-minor-mode)

'cacoo:api-key' は、Cacooにログインして、「設定」＞「APIキー」のページから取得できる文字列です。無くても画像のインライン表示は動きます。
cacoo-mode 起動時に API で Cacoo に接続して、自分が作成した図の一覧を取得します。

## 使い方

テキスト中に以下のように記述して 'cacoo:reload-all-diagrams-command'(C-c , R) すると Cacoo の絵が入ります。

例：Cacooの図

    [img:https://cacoo.com/diagrams/6m4ATG1ddlUiHPqd-0FAF7.png]

'cacoo:anything-command'(C-c , I) すると、Cacooの絵（各シートも含めて）をAnythingで選ぶ画面になります。（ちょっと遅れてプレビュー画像が出ます。）

貼り付けた図はローカルにキャッシュされます。キャッシュは現在のディレクトリ（バッファが保存されるディレクトリ）のなかの「.cimg」というディレクトリに保存されます。オリジナルの図とリサイズされた図が保存されます。

キャッシュがあればネットワークに接続にいきません。リロード 'cacoo:reload-all-diagrams-command' するとキャッシュを消して取りに行きます。リサイズもこのときに行われるので、サイズを変えたいときはリロードしてください。

## キーバインド

'cacoo-minor-mode' が On の時に以下のキーバインドが使えます。また、メニューからも選ぶことが出来ます。

キーバインドを変更する際は 'cacoo-minor-mode-keymap' をカスタマイズしてみてください。

### バッファ全体に対して
- C-c , T : バッファのすべての図をテキストに戻す
- C-c , D : バッファのすべての図を表示する（キャッシュがあればネットワークに接続しない）
- C-c , R : バッファのすべての図を取得し直す

### カーソール直後の図に対して
- C-c , t : テキストに戻す
- C-c , d : 図を表示する（キャッシュがあればネットワークに接続しない）
- C-c , r : 図を取得し直して表示する
- C-c , e : 図の編集画面を表示する（https://cacoo.com/diagrams/xxxxx/edit）
- C-c , v : 図の詳細画面を表示する（https://cacoo.com/diagrams/xxxxx）
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

## 画像取得時のエラーについて

画像の取得や変換中にエラーが起きた場合、該当箇所の色が変わります。また、マウスオーバーで短くエラーメッセージがポップアップで表示されます。

大抵の問題は、 wget が見つからない、 convert が見つからない、URL先の図が見つからないなどだと思います。

## カスタマイズなど

### ブラウザ
ブラウザは 'cacoo:browser-function' で設定してあるものを使います。デフォルトは 'browse-url-browser-function' の値です。

例：Macでsafariを使う場合

    (setq cacoo:browser-function 'browse-url-generic)
    (setq browse-url-generic-program "open")

### 画像ビューアー

外部画像ビューアーのプログラムは 'cacoo:external-viewer' で指定します。nilに設定するとEmacsで開こうとします。

### マークアップ

'cacoo:img-regexp', 'cacoo:img-pattern' を変更することで、Wiki記法などにあわせることができます。'cacoo:img-regexp' は正規表現文字列のリストにもできますので、いくつかの記法を登録しておくことが出来ます。

例：複数登録や貼り付け文字列のカスタマイズ

    (setq cacoo:img-regexp 
         '("\\[img:\\(.*\\)\\][^]\n\r]*$"
           "\\[f:\\(.*\\)\\][^]\n\t]*$"
           "<img src=[\"']\\(.*\\)[\"'][ ]*\\/>[^\n\t]*$"
           ))
    (setq cacoo:img-pattern "<img src=\"%s\" />")

### 画像キャッシュ

キャッシュ用のディレクトリ名は 'cacoo:img-dir' で指定します。
いちいちディレクトリ作成で確認が必要なければ、 'cacoo:img-dir-ok' を t に設定してください。

### 画像サイズ

画像の縮小サイズは 'cacoo:max-size' で指定します。以下のようにスペース区切りで書くことで画像ごとにも指定できます。

例：画像ごとのサイズ指定

    [img:file:///xxx/yyy/zzz.png 600]

### 画像変換

ローカルの画像コピーはデフォルトではcpコマンドを使うようになっています。これは非同期で処理できる部分を増やすことでEmacsのレスポンスを向上させるためです。cpコマンドが使えない場合などEmacsでコピーさせたい場合は 'cacoo:copy-by-command' を nil に設定してください。

Emacs22などではPNGの透過部分が黒くなることがあります。 '(setq cacoo:png-background "white")' を設定に追加して画像のリロードをしてみてください。

Emacsが直接認識できない画像（変数 'image-types' に含まれていない形式やサイズを指定しにくいベクトル形式）を表示させたい場合は、 'cacoo:translation-exts' に拡張子を追加してください。この変数に含まれる画像形式は、ImageMagickでPNG形式に変換して表示しようと試みます。

### プレビュー動作

プレビューの際に作業用のディレクトリを使用します。もしプレビューの際にエラーが出るようであれば 'temporary-file-directory' の値やその値が示すディレクトリを確認してみてください。

### 画像一覧キャッシュ

APIで取得した図やシートの一覧情報は、 'cacoo:api-diagrams-cache-dir' のディレクトリの中の 'cacoo:api-diagrams-cache-file' に保存されます。デフォルトでは '~/.emacs.d/cacoo/diagrams.dat' です。

### 通信

HTTPでの通信はデフォルトでは wget コマンドを使用します。もし、他のコマンドに変えたり、オプションを調整したい場合は 'cacoo:http-get-file-cmd' や 'cacoo:http-get-stdout-cmd' を変更してみてください。

## プラグイン

プラグインによって、その場で画像を生成したり高度なこともできるようになっています。'cacoo-plugins'をrequireされていると使えるようになります。
UMLやシーケンス図、LaTeX数式や、GoogleChartAPI のようなグラフ、また gnuplot や graphviz のようにコマンドラインから図を生成するようなものを本文中に書いて図を表示することが出来ます。org-babel(http://orgmode.org/worg/org-contrib/babel/) の画像版のイメージです。

詳しくはプラグイン周辺のソースの中のコメントを参照してください。

### プラグイン一覧

現在の所以下のようなプラグインを定義しています。

#### URL長文埋め込み
URLに情報を詰めてGETでアクセスすると図を返すようなWebサービスを使うためのプラグインです。

書き方:

    [img:* (url) (filename) (size:省略可)]
    (長文)
    <<<

urlの中の文字列「<<<」がヒアドキュメントのようにURLエンコーディングして埋め込まれます。

例：

    * LaTeX Math 1
    
    [img:* http://maru.bonyari.jp/texclip/render.php/texclip20100511002527.png?s=<<<&f=c&r=300&m=p&b=f texclip1.png 600]
    \begin{align*}
    f(a,b)=\int_a^b\frac{1+x}{a+x^2+x^3}dx
    \end{align*}
    <<<
    
    * LaTeX Math 2
    
    [img:* http://www.codecogs.com/eq.latex?<<< codecogs.png]
    \begin{align*}
    \pi_0(x) =& \sum_{n \ge 1} \frac{\mu(n) J(x^{\frac{1}{n}})}{n} \\
    & J(x) = li (x) - \sum_{\zeta(\rho)=0} li (x^\rho) - \log 2 + \int^{\infty}_{x} \frac{dt}{ t (t^2 -1) \log t}
    \end{align*}
    <<<
    
    * google chart
    
    [img:* http://chart.apis.google.com/chart?cht=p3&chs=250x100&chd=t:60,40&chl=Hello|World chart.png]
    <<<


#### UML埋め込みのプラグイン

http://yuml.me のサービスを使ったUML（クラス図、アクティビティ図、ユースケース図）を生成します。図の書き方などの詳細は http://yuml.me/diagram/class/samples を参照してください。

書き方：

    [img:UML (filename) (size:省略可)]
    UML記述 (http://yuml.me/)
    <<<

例：

    [img:UML uml3.png]
    [Customer]+1->*[Order][Order]++1-items>*[LineItem][Order]-0..1>[PaymentMethod]<<<

#### シーケンス図埋め込みのプラグイン

http://www.websequencediagrams.com/ のサービスを使ったシーケンス図を生成します。書き方の詳細は http://www.websequencediagrams.com/examples.html を参照してください。

書き方：

    [img:SEQ (filename) (size:省略可)]
    シーケンス図記述 (http://www.websequencediagrams.com/)
    <<<

例：

    [img:SEQ uml4.png]
    Alice->Bob: Authentication Request
    note right of Bob: Bob thinks about it.
    Bob-->Alice: Authentication Response
    note over A,B: text1
    note left of A: text2
    note right of A
        multiline
        text
    end note
    A->B: text
    activate B
    B-->A: text
    deactivate B
    <<<

#### はてなフォトライフ記法のプラグイン

はてなフォトライフ記法の画像を取ってくるプラグインです。はてなダイアリーの下書きなどをしているときに便利です。

書き方：

    [f:id:(hatena id):(image id)(ext):image]

使う場合は以下のように画像タグに[f:〜]を追加します。
(setq cacoo:img-regexp 
     '("\\[img:\\(.*\\)\\][^]\n\r]*$"
       "\\[f:\\(.*\\)\\][^]\n\t]*$"))

例：

    [f:id:kiwanami:20100527231749p:image]


#### コマンド起動埋め込みのプラグイン

シェルからコマンドを実行して図を生成するときに使うプラグインです。

ちょっとややこしいですが、以下のような動作になります。

- 一時ファイルに内容を書き出す
  - そのとき出力画像ファイル名を %OUT% に展開
- コマンドを実行する
  - そのとき上の一時ファイル名を %IN% に展開
- 出力画像がキャッシュディレクトリ上の filename にできあがる
- あとは適当なサイズにリサイズされて表示

書き方：

    [img:CMD "(command)" (filename) (size:省略可)]
    ファイルとして書き出すもの
    <<<

commandや書き出し文字列の中の %IN% がテンポラリファイル名、%OUT%が出力ファイル名に入れ替わります。以下の例を参照してみてください。

例：

    * gnuplot
    
    [img:CMD 'gnuplot %IN%' plot1.eps]
    set term postscript eps
    set output '%OUT%'
    plot exp(-0.03*x*x)*sin(x)
    <<<
    
    * R
    
    [img:CMD 'R CMD BATCH %IN%' plot2.eps]
    postscript("%OUT%")
    image(volcano)
    dev.off()
    <<<


#### Graphviz図埋め込みのプラグイン

Graphvizで簡単なグラフ図を書くことが出来ます。

書き方：

    [img:DOT (filename) (size:省略可)]
    dot記述 (http://www.graphviz.org/)
    <<<

例：

    [img:DOT graphviz.png]
    digraph sample {
       alpha -> beta;
       alpha -> 日本語;
       beta -> delta;
    }
    <<<

#### SVG

SVGをインラインで書くことが出来ます。（EmacsでSVGがサポートされている必要があります。）
SVGをEmacsでがりがり書くのであれば image-mode で書く方が便利だと思います。

書き方：

    [img:SVG (filename) (size:省略可)]
    SVG記述
    <<<

例：

    [img:SVG sample1.svg]
    <?xml version="1.0" standalone="no"?>
    <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" 
    	"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
    
    <svg width="200" height="200" xmlns="http://www.w3.org/2000/svg">
    <defs>
        <circle id="C" cx="20" cy="20" r="20" style="fill:red;" />
      </defs>
      <use xlink:href="#C" x="20" y="20" />
      <use xlink:href="#C" x="80" y="80" />
      <use xlink:href="#C" x="140" y="140" />
    </svg>
    <<<


## ライセンスなど

- License:  GPL v3
- Repository:  http://github.com/kiwanami/emacs-cacoo
- SAKURAI, Masashi (m.sakurai atmark kiwanami.net)
