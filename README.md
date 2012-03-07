# judge-indent.el --- judge indent and tab widths

`judge-indent-mode` は、ファイルを開いた瞬間、
以下の 9 (厳密には 7) パターンの中からインデント幅およびタブ幅を自動的に判定します。

      \  indent
       \  2 4 8
    tab \------
      2 | U
      4 | X U
      8 | X X U <- 3 つの `U' は判別できない。
      - | X X X

判定方法は、行頭の 2 個スペース、4 個スペース、8 個スペース、1 個タブを数えて、それらを比べるというものです。
そして、そのインデントスタイルに合うように Emacs の挙動が変化します。
これによって、既存のインデントスタイルを乱さずに、
他人・チームのコードに手を入れることが可能となります。

# 使い方

以下の 3 行を設定ファイルに追加してください。
`c-mode`、`python-mode`、`sh-mode` で使用する例です。

    (require 'judge-indent)
    (global-judge-indent-mode t)
    (setq judge-indent-major-modes '(c-mode python-mode sh-mode))

# カスタマイズ

デフォルトのインデント幅 (2 か 4 か 8) を設定する。
デフォルト: `c-basic-offset` のデフォルト値、もしくは 4。

    (setq judge-indent-default-indent-width 2)

デフォルトのタブ幅 (2 か 4 か 8) を設定する。
デフォルト: `tab-width` のデフォルト値、もしくは 8。

    (setq judge-indent-default-tab-width 4)

既存コードのインデントがあまり深くないときにタブを好むかどうかのフラグを設定する。
デフォルト: `indent-tabs-width` のデフォルト値、もしくは `nil`。

    (setq judge-indent-prefer-tabs-mode t)

インデント幅とタブ幅を判定するときに用いる相対許容誤差 [%] を設定する。
デフォルト: 0 %。

    (setq judge-indent-relative-tolerance 3)

大きいサイズのファイルに対して、判定に用いる文字数を設定する。
デフォルト: 30000 文字 (＝約 1000 行)。

    (setq judge-indent-search-limit 60000)

インデント幅を保持する変数を追加設定する。

    (add-to-list 'judge-indent-variables-indent-width 'c-basic-offset)

# 関数

マイナーモードをオン／オフにする。

* judge-indent-mode

バッファ／リージョンからインデント幅およびタブ幅を判定する。

* judge-indent-buffer
* judge-indent-region

インデント幅およびタブ幅を手動で設定 (かつ、既存コードを整形) する。

* judge-indent-{set,set-apply}-indent-tab-widths
* judge-indent-{set,set-apply}-default-indent-tab-widths
* judge-indent-{set,set-apply}-indent-width{2,4,8}-tab-{disabled,width{2,4,8}}

インデント幅を手動で設定 (かつ、既存コードを整形) する。

* judge-indent-{set,set-apply}-indent-width
* judge-indent-{set,set-apply}-default-indent-width
* judge-indent-{set,set-apply}-indent-width{2,4,8}

タブ幅を手動で設定 (かつ、既存コードを整形) する。

* judge-indent-{set,set-apply}-tab-width
* judge-indent-{set,set-apply}-default-tab-width
* judge-indent-{set,set-apply}-tab-{disabled,width{2,4,8}}

バッファ／リージョン内のインデントをカウントして表示する。

* judge-indent-message-indent-counts-buffer
* judge-indent-message-indent-counts-region

# バージョン

## 1.1.1 on 29 Feb. 2012

既存コードを整形する関数。インデント幅を保持する変数の追加設定。リファクタリング。

## 1.1.0 on 2 July 2011

精度向上。リージョンに関する関数。リファクタリング。

## 1.0.0 on 26 June 2011

ファーストコミット。

# 既知の問題
